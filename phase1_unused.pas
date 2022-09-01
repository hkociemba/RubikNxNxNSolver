unit phase1_unused;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cubedefs;

procedure createUDBrickCoordToSymCoordTable;
procedure createUDBrickCoordSymTransTable;
procedure createUDBrickXYPruningTable;
procedure createUDBrickXCentXPruningTable;
procedure createUDCentXYPruningTable;

var
   UDBrickCoordToSymCoord: array of SymCoord32;
   SymCoordRepToUDBrickCoord: array of Int32;
   UDBrickXYPrun: array of array of UInt32;
   UDCentXYPrun: array of array of UInt32;
   UDBrickXCentXPrun: array of array of UInt32;

   UDBrickCoordSymTransform: array of array of integer;

implementation

uses phase1_tables,facecube, main, forms;


procedure createUDBrickCoordToSymCoordTable;
//The UD bricks have 16-fold symmetry
var
  fc: faceletCube;
var
  i, j, k, m, sym, repIdx, coord: integer;
  Free: UInt32;
begin
  SetLength(UDBrickCoordToSymCoord, B_24_8);
  SetLength(SymCoordRepToUDBrickCoord, N_SYMBRICKCOORD);
  // 46935 equivalence classes
  fc := faceletCube.Create(nil, 11); //11 more or less arbitrary
  Free := High(UInt32);
  for k := 0 to B_24_8 - 1 do
  begin
    UDBrickCoordToSymCoord[k].c_idx := Free;
    UDBrickCoordToSymCoord[k].sym := 0;
  end;
  repIdx := 0;
  for k := 0 to B_24_8 - 1 do
  begin
    if UDBrickCoordToSymCoord[k].c_idx = Free then   //new representant
    begin
      SymCoordRepToUDBrickCoord[repIdx] := k;
      // Für Pruning Table Erstellung nötig
      fc.InvPhase1CenterCoord(k, 2, 5); // 2 arbitrary, 11 div 2 important
      for m := 0 to 1 do
      begin
        for i := 0 to 1 do
        begin
          for j := 0 to 3 do
          begin
            sym := 8 * m + 4 * i + j; //0<=sym<16
            coord := fc.Phase1CenterCoord(2, 5);

            UDBrickCoordToSymCoord[coord].c_idx := repIdx;
            UDBrickCoordToSymCoord[coord].sym :=
              UDBrickCoordToSymCoord[coord].sym or (1 shl sym);
            //apply all 16 symmetries to the representant and fill up to 16 raw coord
            //entries. If  the representant has symmetry itself, we have less entries
            // each containing 2, 4, 8 or 16 sym bits.
            fc.applySymmetry(2, 5, S_U4);
          end;
          fc.applySymmetry(2, 5, S_F2);
        end;
        fc.applySymmetry(2, 5, S_LR2);
      end;
      Inc(repIdx);
    end;
  end;
  fc.Free;
end;

// UDBrickCoordSymTransform[idx,s] gives UDBrickCoord after applying
// the inverse of symmetry s
procedure createUDBrickCoordSymTransTable;
var
  fc: faceletCube;
  i, j: integer;
  fs: TFileStream;
const
  fName = 'UDBrickTrans';
begin
  SetLength(UDBrickCoordSymTransform, B_24_8, 16);
  if FileExists(fName) then
  begin
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := 0 to B_24_8 - 1 do
      fs.ReadBuffer(UDBrickCoordSymTransform[i][0], 16 * SizeOf(integer));
    fs.Free;
  end
  else
  begin
    fc := faceletCube.Create(nil, 11);
    for i := 0 to B_24_8 - 1 do
    begin
      fc.InvPhase1CenterCoord(i, 2, 5);
      for j := 0 to 15 do
      begin
        fc.applyInvSymmetryByIndex(2, 5, j);
        UDBrickCoordSymTransform[i, j] := fc.Phase1CenterCoord(2, 5);
        fc.applySymmetryByIndex(2, 5, j);//restore state
      end;
    end;
    fc.Free;
    fs := TFileStream.Create(fName, fmCreate);
    for i := 0 to B_24_8 - 1 do
      fs.WriteBuffer(UDBrickCoordSymTransform[i][0], 16 * SizeOf(integer));
    fs.Free;
  end;
end;

procedure set_bxby_depth3(bx_class, by: integer; val: UInt32);
var
  base, shift, v: UInt32;
begin
  base := by shr 4;
  shift := (by mod 16) * 2;

  v := UDBrickXYPrun[bx_class, base] and (not (3 shl shift));
  v := v or (val shl shift);
  UDBrickXYPrun[bx_class, base] := v;
end;

function get_bxby_depth3(bx_class, by: integer): integer;
var
  base, shift, y: UInt32;
begin
  base := by shr 4;
  shift := (by mod 16) * 2;
  y := UDBrickXYPrun[bx_class, base];
  Result := (y shr shift) and 3;
end;

procedure createUDBrickXYPruningTable;
var
  i, j, depth, bx_classidx, bx, bx1, bx1_class, bx1_sym, by, by1,
  altby1, depth3: UInt32;
  done, total: UInt64;
  backsearch, match: boolean;
  fs: TFileStream;
  m: Moves;

const
  fName = 'UDBrickXYPrun';
begin
  total := UInt64(B_24_8) * N_SYMBRICKCOORD;

  SetLength(UDBrickXYPrun, N_SYMBRICKCOORD);
  Form1.Memo1.Lines.Add(Format('Initializing 8.5 GB of Memory...', []));
  for i := 0 to N_SYMBRICKCOORD - 1 do
  begin
    Application.ProcessMessages;
    SetLength(UDBrickXYPrun[i], N_CHUNK);
  end;

  if FileExists(fName) then
  begin
    Form1.Memo1.Lines.Add(Format('Loading pruning table %s', [fName]));
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := 0 to N_SYMBRICKCOORD - 1 do
    begin
      Application.ProcessMessages;
      fs.ReadBuffer(UDBrickXYPrun[i][0], N_CHUNK * SizeOf(UInt32));
    end;
    fs.Free;
    Form1.Memo1.Lines.Add(Format('Done!', []));
  end
  else
  begin
    Form1.Memo1.Lines.Add(Format('Generating UDBrickXBrickYPrun pruning table...', []));
    Form1.Memo1.Lines.Add(Format('This will take several hours.', []));
    for i := 0 to N_SYMBRICKCOORD - 1 do
      for j := 0 to N_CHUNK - 1 do
        UDBrickXYPrun[i, j] := $ffffffff;

    bx_classidx := 0;
    by := 0;
    set_bxby_depth3(bx_classidx, by, 0);
    done := 1;
    depth := 0;
    backsearch := False;

    while done <> total do
    begin
      depth3 := depth mod 3;
      if depth = 13 then  //12 or 13 are good choices
      begin
        Form1.Memo1.Lines.Add(
          Format('Flipping to backward search at depth %d.', [depth]));
        backsearch := True;
      end;
      for bx_classidx := 0 to N_SYMBRICKCOORD - 1 do
      begin
        Application.ProcessMessages;

        by := 0;
        while by < B_24_8 do
        begin
          if (not backsearch) and (by mod 16 = 0) and
            (UDBrickXYPrun[bx_classidx, by div 16] = $FFFFFFFF) and
            (by < B_24_8 - 16) then
          begin
            Inc(by, 16);
            continue;
          end;
          if backsearch then
            match := (get_bxby_depth3(bx_classidx, by) = 3)
          else
            match := (get_bxby_depth3(bx_classidx, by) = depth3);

          if match then
          begin
            bx := SymCoordRepToUDBrickCoord[bx_classidx];

            for m := fU1 to yB3 do
            begin
              case m of
                fU1..fB3:
                begin//face moves move all bricks
                  bx1 := UDCenterMove[bx, Ord(m)];
                  by1 := UDCenterMove[by, Ord(m)];
                end;
                xU1..xB3:
                begin//x-moves move only x-bricks
                  bx1 := UDCenterMove[bx, Ord(m)];
                  by1 := by;
                end;
                yU1..yB3:
                begin//y-moves move only y-bricks. Caveat: it is essentially
                  // an x move of a different orbit!
                  bx1 := bx;
                  by1 := UDCenterMove[by, Ord(m) - 18];//-18 not necessary though
                end;
              end;
              bx1_class := UDBrickCoordToSymCoord[bx1].c_idx;
              bx1_sym := UDBrickCoordToSymCoord[bx1].sym;

              if not backsearch then
              begin

                for i := 0 to 15 do //more than one symmetry possible
                begin
                  if (bx1_sym and (1 shl i)) <> 0 then
                  begin
                    altby1 := UDBrickCoordSymTransform[by1, i];

                    if get_bxby_depth3(bx1_class, altby1) = 3 then //entry empty
                    begin
                      set_bxby_depth3(bx1_class, altby1, (depth + 1) mod 3);
                      Inc(done);
                      //Form1.Memo1.Lines.Add(Format('classidx: %d, by: %d, done: %d', [bx1_class,altby1,done]));
                    end;
                  end;
                end;
              end
              else //backwards search
              begin
                i := 0;//find one symmetry
                while (bx1_sym and (1 shl i)) = 0 do
                  Inc(i);
                altby1 := UDBrickCoordSymTransform[by1, i];
                if get_bxby_depth3(bx1_class, altby1) = depth3 then //entry set
                begin
                  set_bxby_depth3(bx_classidx, by, (depth + 1) mod 3);
                  Inc(done);

                  if done > total - 70 then
                  begin
                    Form1.Memo1.Lines.Add(Format('bx: %d, by: %d, depth: %d, ',
                      [bx, by, depth + 1]));
                    j := 0;
                    for i := 0 to 15 do
                    begin
                      if (UDBrickCoordToSymCoord[bx].sym and (1 shl i)) <> 0 then
                        Inc(j);
                    end;
                    Form1.Memo1.Lines.Add(Format('symmetries: %d', [j]));
                  end;

                  break;//no more m
                end;
              end;

            end;//m
          end;//match

          Inc(by);
        end;
      end;//bx_classidx
      Form1.Memo1.Lines.Add(Format('depth: %d, done: %d', [depth, done]));
      Application.ProcessMessages;
      Inc(depth);
    end;
    fs := TFileStream.Create(fName, fmCreate);
    for i := 0 to N_SYMBRICKCOORD - 1 do
      fs.WriteBuffer(UDBrickXYPrun[i][0], N_CHUNK * SizeOf(UInt32));
    fs.Free;
  end;
end;


procedure set_bxcx_depth3(bx_class, by: integer; val: UInt32);
var
  base, shift, v: UInt32;
begin
  base := by shr 4;
  shift := (by mod 16) * 2;

  v := UDBrickXCentXPrun[bx_class, base] and (not (3 shl shift));
  v := v or (val shl shift);
  UDBrickXCentXPrun[bx_class, base] := v;
end;

function get_bxcx_depth3(bx_class, by: integer): integer;
var
  base, shift, y: UInt32;
begin
  base := by shr 4;
  shift := (by mod 16) * 2;
  y := UDBrickXCentXPrun[bx_class, base];
  Result := (y shr shift) and 3;
end;


procedure createUDBrickXCentXPruningTable;
//table holds for x-bricks and (x,y)-orbit and also for (y-bricks) and (y,x)-orbit
var
  i, j, depth, bx_classidx, bx, bx1, bx1_class, bx1_sym, by, by1,
  altby1, depth3: UInt32;
  done, total: UInt64;
  backsearch, match: boolean;
  fs: TFileStream;
  m: Moves;

const
  fName = 'UDBrickXCentXPrun';
begin
  total := UInt64(B_24_8) * N_SYMBRICKCOORD;

  SetLength(UDBrickXCentXPrun, N_SYMBRICKCOORD);
  Form1.Memo1.Lines.Add(Format('Initializing another 8.5 GB of Memory...', []));
  for i := 0 to N_SYMBRICKCOORD - 1 do
  begin
    Application.ProcessMessages;
    SetLength(UDBrickXCentXPrun[i], N_CHUNK);
  end;

  if FileExists(fName) then
  begin
    Form1.Memo1.Lines.Add(Format('Loading pruning table %s', [fName]));
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := 0 to N_SYMBRICKCOORD - 1 do
    begin
      Application.ProcessMessages;
      fs.ReadBuffer(UDBrickXCentXPrun[i][0], N_CHUNK * SizeOf(UInt32));
    end;
    fs.Free;
    Form1.Memo1.Lines.Add(Format('Done!', []));
  end
  else
  begin
    Form1.Memo1.Lines.Add(Format('Generating pruning table %s', [fName]));
    Form1.Memo1.Lines.Add(Format('This will take several hours.', []));
    for i := 0 to N_SYMBRICKCOORD - 1 do
      for j := 0 to N_CHUNK - 1 do
        UDBrickXCentXPrun[i, j] := $ffffffff;

    bx_classidx := 0;
    by := 0;
    set_bxcx_depth3(bx_classidx, by, 0);
    done := 1;
    depth := 0;
    backsearch := False;

    while done <> total do
    begin
      depth3 := depth mod 3;
      if depth = 12 then  //12 seem appropriate
      begin
        Form1.Memo1.Lines.Add(
          Format('Flipping to backward search at depth %d.', [depth]));
        backsearch := True;
      end;
      for bx_classidx := 0 to N_SYMBRICKCOORD - 1 do
      begin
        Application.ProcessMessages;

        by := 0;
        while by < B_24_8 do
        begin
          if (not backsearch) and (by mod 16 = 0) and
            (UDBrickXCentXPrun[bx_classidx, by div 16] = $FFFFFFFF) and
            (by < B_24_8 - 16) then
          begin
            Inc(by, 16);
            continue;
          end;
          if backsearch then
            match := (get_bxcx_depth3(bx_classidx, by) = 3)
          else
            match := (get_bxcx_depth3(bx_classidx, by) = depth3);

          if match then
          begin
            bx := SymCoordRepToUDBrickCoord[bx_classidx];


            for m := fU1 to yB3 do
            begin
              case m of
                fU1..fB3:
                begin//face moves move x-bricks and (x,y)-orbit centers
                  bx1 := UDCenterMove[bx, Ord(m)];
                  by1 := UDCenterMove[by, Ord(m)];
                end;
                xU1..xB3:
                begin//x-moves move x-bricks and (x,y)-orbit centers
                  bx1 := UDCenterMove[bx, Ord(m)];
                  by1 := UDCenterMove[by, Ord(m)];
                end;
                yU1..yB3:
                begin//y-moves move only (x,y)-orbit centers
                  bx1 := bx;
                  by1 := UDCenterMove[by, Ord(m)];
                end;
              end;
              bx1_class := UDBrickCoordToSymCoord[bx1].c_idx;
              bx1_sym := UDBrickCoordToSymCoord[bx1].sym;

              if not backsearch then
              begin

                for i := 0 to 15 do //more than one symmetry possible
                begin
                  if (bx1_sym and (1 shl i)) <> 0 then
                  begin
                    altby1 := UDBrickCoordSymTransform[by1, i];

                    if get_bxcx_depth3(bx1_class, altby1) = 3 then //entry empty
                    begin
                      set_bxcx_depth3(bx1_class, altby1, (depth + 1) mod 3);
                      Inc(done);
                      //Form1.Memo1.Lines.Add(Format('classidx: %d, by: %d, done: %d', [bx1_class,altby1,done]));
                    end;
                  end;
                end;
              end
              else //backwards search
              begin
                i := 0;//find one symmetry
                while (bx1_sym and (1 shl i)) = 0 do
                  Inc(i);
                altby1 := UDBrickCoordSymTransform[by1, i];
                if get_bxcx_depth3(bx1_class, altby1) = depth3 then //entry set
                begin
                  set_bxcx_depth3(bx_classidx, by, (depth + 1) mod 3);
                  Inc(done);

                  if done > total - 70 then
                  begin
                    Form1.Memo1.Lines.Add(Format('bx: %d, by: %d, depth: %d, ',
                      [bx, by, depth + 1]));
                    j := 0;
                    for i := 0 to 15 do
                    begin
                      if (UDBrickCoordToSymCoord[bx].sym and (1 shl i)) <> 0 then
                        Inc(j);
                    end;
                    Form1.Memo1.Lines.Add(Format('symmetries: %d', [j]));
                  end;
                  break;//no more m
                end;
              end;

            end;//m
          end;//match

          Inc(by);
        end;
      end;//bx_classidx
      Form1.Memo1.Lines.Add(Format('depth: %d, done: %d', [depth, done]));
      Application.ProcessMessages;
      Inc(depth);
    end;
    fs := TFileStream.Create(fName, fmCreate);
    for i := 0 to N_SYMBRICKCOORD - 1 do
      fs.WriteBuffer(UDBrickXCentXPrun[i][0], N_CHUNK * SizeOf(UInt32));
    fs.Free;
  end;
end;

procedure set_cxcy_depth3(cx_class, cy: integer; val: UInt32);
var
  base, shift, v: UInt32;
begin
  base := cy shr 4;
  shift := (cy mod 16) * 2;

  v := UDCentXYPrun[cx_class, base] and (not (3 shl shift));
  v := v or (val shl shift);
  UDCentXYPrun[cx_class, base] := v;
end;

function get_cxcy_depth3(cx_class, cy: integer): integer;
var
  base, shift, y: UInt32;
begin
  base := cy shr 4;
  shift := (cy mod 16) * 2;
  y := UDCentXYPrun[cx_class, base];
  Result := (y shr shift) and 3;
end;


procedure createUDCentXYPruningTable;
var
  i, j, depth, cx_classidx, cx, cx1, cx1_class, cx1_sym, cy, cy1,
  altcy1, depth3: UInt32;
  done, total: UInt64;
  backsearch, match: boolean;
  fs: TFileStream;
  m: Moves;

const
  fName = 'UDCentXYPrun';
begin
  total := UInt64(B_24_8) * N_SYMCENTCOORD;

  SetLength(UDCentXYPrun, N_SYMCENTCOORD);
  Form1.Memo1.Lines.Add(Format('Initializing 17 GB of Memory...', []));
  for i := 0 to N_SYMCENTCOORD - 1 do
  begin
    Application.ProcessMessages;
    SetLength(UDCentXYPrun[i], N_CHUNK);
  end;

  if FileExists(fName) then
  begin
    Form1.Memo1.Lines.Add(Format('Loading pruning table %s', [fName]));
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := 0 to N_SYMCENTCOORD - 1 do
    begin
      Application.ProcessMessages;
      fs.ReadBuffer(UDCentXYPrun[i][0], N_CHUNK * SizeOf(UInt32));
    end;
    fs.Free;
    Form1.Memo1.Lines.Add(Format('Done!', []));
  end
  else
  begin
    Form1.Memo1.Lines.Add(Format('Generating %s pruning table...', [fName]));
    Form1.Memo1.Lines.Add(Format('This will take several hours.', []));
    for i := 0 to N_SYMCENTCOORD - 1 do
      for j := 0 to N_CHUNK - 1 do
        UDCentXYPrun[i, j] := $ffffffff;

    cx_classidx := 0;
    cy := 0;
    set_cxcy_depth3(cx_classidx, cy, 0);
    done := 1;
    depth := 0;
    backsearch := False;

    while done <> total do
    begin
      depth3 := depth mod 3;
      if depth = 10 then  //13 are good choices  { TODO : muss bestimmt werden }
      begin
        Form1.Memo1.Lines.Add(
          Format('Flipping to backward search at depth %d.', [depth]));
        backsearch := True;
      end;
      for cx_classidx := 0 to N_SYMCENTCOORD - 1 do
      begin
        Application.ProcessMessages;

        cy := 0;
        while cy < B_24_8 do
        begin
          if (not backsearch) and (cy mod 16 = 0) and
            (UDCentXYPrun[cx_classidx, cy div 16] = $FFFFFFFF) and
            (cy < B_24_8 - 16) then
          begin
            Inc(cy, 16);
            continue;
          end;
          if backsearch then
            match := (get_cxcy_depth3(cx_classidx, cy) = 3)
          else
            match := (get_cxcy_depth3(cx_classidx, cy) = depth3);

          if match then
          begin
            cx := SymCoordRepToUDCenterCoord[cx_classidx];

            for m := fU1 to yB3 do  { TODO : Bedingung für moves machen }
            begin
              case m of
                fU1..fB3:
                begin
                  cx1 := UDCenterMove[cx, Ord(m)];
                  cy1 := UDCenterMove[cy, Ord(m)];
                end;
                xU1..xB3:
                begin
                  cx1 := UDCenterMove[cx, Ord(m)];
                  cy1 := UDCenterMove[cy, Ord(m)+18];
                end;
                yU1..yB3:
                begin
                  cx1 := UDCenterMove[cx, Ord(m)];
                  cy1 := UDCenterMove[cy, Ord(m) - 18];
                end;
              end;
              cx1_class := UDCentCoordToSymCoord[cx1].c_idx;
              cx1_sym := UDCentCoordToSymCoord[cx1].sym;

              if not backsearch then
              begin

                for i := 0 to 7 do //more than one symmetry possible
                begin
                  if (cx1_sym and (1 shl i)) <> 0 then
                  begin
                    altcy1 := UDCentCoordSymTransform[cy1, i];

                    if get_cxcy_depth3(cx1_class, altcy1) = 3 then //entry empty
                    begin
                      set_cxcy_depth3(cx1_class, altcy1, (depth + 1) mod 3);
                      Inc(done);
                    end;
                  end;
                end;
              end
              else //backwards search
              begin
                i := 0;//find one symmetry
                while (cx1_sym and (1 shl i)) = 0 do
                  Inc(i);
                altcy1 := UDCentCoordSymTransform[cy1, i];
                if get_cxcy_depth3(cx1_class, altcy1) = depth3 then //entry set
                begin
                  set_cxcy_depth3(cx_classidx, cy, (depth + 1) mod 3);
                  Inc(done);

                  if done > total - 70 then
                  begin
                    Form1.Memo1.Lines.Add(Format('cx: %d, cy: %d, depth: %d, ',
                      [cx, cy, depth + 1]));
                    j := 0;
                    for i := 0 to 7 do
                    begin
                      if (UDCentCoordToSymCoord[cx].sym and (1 shl i)) <> 0 then
                        Inc(j);
                    end;
                    Form1.Memo1.Lines.Add(Format('symmetries: %d', [j]));
                  end;

                  break;//no more m
                end;
              end;

            end;//m
          end;//match

          Inc(cy);
        end;
      end;//cx_classidx
      Form1.Memo1.Lines.Add(Format('depth: %d, done: %d', [depth, done]));
      Application.ProcessMessages;
      Inc(depth);
    end;
    fs := TFileStream.Create(fName, fmCreate);
    for i := 0 to N_SYMCENTCOORD - 1 do
      fs.WriteBuffer(UDCentXYPrun[i][0], N_CHUNK * SizeOf(UInt32));
    fs.Free;
  end;
end;
end.

