unit phase3_tables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cubedefs;

procedure createNextMovePhase3Table;
//procedure createPh3CenterMoveTable;

procedure createPh3RLFBCenterMoveTable;
procedure createPh3RLFBXCrossMoveTable;

procedure createPh3Brick702CoordToSymCoordTable;

procedure createPh3RLFBCenterCoordSymTransTable;

procedure createPh3Brick702RLFBCentPruningTable;

procedure createPh3RLFBXCrossPruningTable;
procedure createPh3RLFBPlusCrossPruningTable;

function get_bycx_depth3(b_class, c: integer): integer;

var
  Ph3faceMoveAllowed: array of array of boolean;
  nextMovePhase3Arr: array [fU1 .. NoMove, InitMove .. yB3] of moves;

  Ph3RLFBCenterMove: array of array of UInt16;
  Ph3RLFBXCrossMove: array of array of UInt16;

  Ph3Brick702CoordToSymCoord: array of SymCoord16;
  SymCoordRepToPh3Brick702Coord: array of UInt16;

  Ph3RLFBCentCoordSymTransform: array of array of UInt16;

  Ph3BrickRLFBCentPrun: array of array of UInt32;
  Ph3Brick702RLFBCentPrun: array of array of UInt32;

  Ph3RLFBXCrossPrun: array of byte;
  Ph3RLFBPlusCrossPrun: array of byte;


implementation

uses facecube, main, Forms;

procedure createNextMovePhase3Table;
var
  fc: faceletCube;
  mprev, mcurr: moves;
begin
  fc := faceletCube.Create(nil, 11); // any odd value possible
  for mcurr := InitMove to yB3 do
    // use NoMove for the predecessor if we have the first move
  begin
    if not Phase3Allowed[Ord(mcurr)] then
      continue;
    nextMovePhase3Arr[NoMove, mcurr] := fc.nextMovePh3(0, mcurr);
  end;
  for mprev := fU1 to yB3 do
  begin
    fc.fxymoves[0] := mprev;
    for mcurr := InitMove to yB3 do
    begin
      if not Phase3Allowed[Ord(mcurr)] then
        continue;
      nextMovePhase3Arr[mprev, mcurr] := fc.nextMovePh3(1, mcurr);

    end;
  end;
  fc.Free;
end;

procedure createPh3RLFBCenterMoveTable;
var
  i, k: integer;
  a: Axis;
  n: UInt16;
  fs: TFileStream;
  fc: faceletcube;
const
  fName = 'Ph3RLFBCenterMove';
begin
  fc := faceletCube.Create(nil, 11); // 11 arbitrary
  SetLength(Ph3RLFBCenterMove, B_8_4 * B_8_4, 3 * 18);

  if FileExists(fName) then
  begin
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := Low(Ph3RLFBCenterMove) to High(Ph3RLFBCenterMove) do
      fs.ReadBuffer(Ph3RLFBCenterMove[i][0], SizeOf(UInt16) * 3 * 18);
    fs.Free;
  end
  else
  begin
    n := High(UInt16);
    for i := 0 to B_8_4 * B_8_4 - 1 do
      for k := 0 to 53 do
        Ph3RLFBCenterMove[i, k] := n;

    for i := 0 to B_8_4 * B_8_4 - 1 do
    begin
      fc.InvPh3RLFBCenterCoord(i, 2, 3);//2, 3 arbitrary
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 0); // Faceturns
          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k] then
            Ph3RLFBCenterMove[i, 3 * Ord(a) + k] :=
              fc.Ph3RLFBCenterCoord(2, 3);
        end;
      end;
    end;

    for i := 0 to B_8_4 * B_8_4 - 1 do
    begin
      fc.InvPh3RLFBCenterCoord(i, 2, 3); // U,R,F: 0,2,4
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 2); //x moves
          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k + 18] then
            Ph3RLFBCenterMove[i, 3 * Ord(a) + k + 18] :=
              fc.Ph3RLFBCenterCoord(2, 3);
        end;
      end;
    end;

    for i := 0 to B_8_4 * B_8_4 - 1 do
    begin
      fc.InvPh3RLFBCenterCoord(i, 2, 3);
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 3);//y moves
          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k + 36] then
            Ph3RLFBCenterMove[i, 3 * Ord(a) + k + 36] :=
              fc.Ph3RLFBCenterCoord(2, 3);
        end;
      end;
    end;
    fs := TFileStream.Create(fName, fmCreate);
    for i := Low(Ph3RLFBCenterMove) to High(Ph3RLFBCenterMove) do
      fs.WriteBuffer(Ph3RLFBCenterMove[i][0], SizeOf(UInt16) * 3 * 18);
    fs.Free;
  end;
  fc.Free;
end;

procedure createPh3RLFBXCrossMoveTable;
var
  fc: faceletcube;
  i, j, k: integer;
  a: axis;
  n: UInt16;
  fs: TFileStream;

const
  fName = 'Ph3RLFBXCrossMove';
begin
  fc := faceletCube.Create(nil, 11); // 11 arbitrary
  SetLength(Ph3RLFBXCrossMove, B_8_4 * B_8_4, 2 * 18);

  if FileExists(fName) then
  begin
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := Low(Ph3RLFBXCrossMove) to High(Ph3RLFBXCrossMove) do
      fs.ReadBuffer(Ph3RLFBXCrossMove[i][0], SizeOf(UInt16) * 2 * 18);
    fs.Free;
  end
  else
  begin
    n := High(UInt16);
    for i := 0 to 4900 - 1 do
      for j := 0 to 35 do
        Ph3RLFBXCrossMove[i, j] := n;


    for i := 0 to 4900 - 1 do
    begin
      fc.InvPh3RLFBCenterCoord(i, 2, 2);//2 arbitrary
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 0); // face turns
          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k] then
            Ph3RLFBXCrossMove[i, 3 * Ord(a) + k] :=
              fc.Ph3RLFBCenterCoord(2, 2);
        end;
      end;
    end;


    for i := 0 to 4900 - 1 do
    begin
      fc.InvPh3RLFBCenterCoord(i, 2, 2);
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 2); //only x moves
          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k + 18] then
            Ph3RLFBXCrossMove[i, 3 * Ord(a) + k + 18] :=
              fc.Ph3RLFBCenterCoord(2, 2);
        end;
      end;
    end;
    fs := TFileStream.Create(fName, fmCreate);
    for i := Low(Ph3RLFBXCrossMove) to High(Ph3RLFBXCrossMove) do
      fs.WriteBuffer(Ph3RLFBXCrossMove[i][0], SizeOf(UInt16) * 2 * 18);
    fs.Free;

  end;
  fc.Free;
end;

//procedure createPh3CenterMoveTable;
//var
//  i, j, k: integer;
//  a: Axis;
//  n: UInt32;
//  fc: faceletcube;
//begin
//  fc := faceletCube.Create(nil, 11); // 11 arbitrary
//  n := High(UInt32);
//  SetLength(Phase3CenterMove, 3, B_8_4, 3 * 18);
//  //first coordinate direction 0,1,2 for UD RL FB
//  // einige moves are not allowed
//
//
//  for j := 0 to 2 do
//    for i := 0 to B_8_4 - 1 do
//      for k := 0 to 53 do
//        Phase3CenterMove[j, i, k] := n;
//
//
//  for j := 0 to 2 do
//    for i := 0 to B_8_4 - 1 do
//    begin
//      fc.InvPhase3CenterCoord(i, 2, 3, Axis(2 * j));//2, 3 arbitrary
//      for a := U to B do
//      begin
//        for k := 0 to 3 do
//        begin
//          fc.move(a, 0); // Faceturns
//          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k] then
//            Phase3CenterMove[j, i, 3 * Ord(a) + k] :=
//              fc.Phase3CenterCoord(2, 3, Axis(2 * j));
//        end;
//      end;
//    end;
//
//
//  for j := 0 to 2 do
//    for i := 0 to B_8_4 - 1 do
//    begin
//      fc.InvPhase3CenterCoord(i, 2, 3, Axis(2 * j)); // U,R,F: 0,2,4
//      for a := U to B do
//      begin
//        for k := 0 to 3 do
//        begin
//          fc.move(a, 2); //x moves
//          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k + 18] then
//            Phase3CenterMove[j, i, 3 * Ord(a) + k + 18] :=
//              fc.Phase3CenterCoord(2, 3, Axis(2 * j));
//        end;
//      end;
//    end;
//
//  for j := 0 to 2 do
//    for i := 0 to B_8_4 - 1 do
//    begin
//      fc.InvPhase3CenterCoord(i, 2, 3, Axis(2 * j));
//      for a := U to B do
//      begin
//        for k := 0 to 3 do
//        begin
//          fc.move(a, 3);//y moves
//          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k + 36] then
//            Phase3CenterMove[j, i, 3 * Ord(a) + k + 36] :=
//              fc.Phase3CenterCoord(2, 3, Axis(2 * j));
//        end;
//      end;
//    end;
//
//  fc.Free;
//end;


procedure createPh3Brick702CoordToSymCoordTable;
//The bricks have 48-fold symmetry, we use only 8 symmetries
var
  fc: faceletCube;
var
  i, j, k, sym, repIdx, coord: integer;
  Free: UInt16;
begin
  SetLength(Ph3Brick702CoordToSymCoord, 4900);
  SetLength(SymCoordRepToPh3Brick702Coord, 690);{ TODO : anpassen }
  // 690 equivalence classes
  fc := faceletCube.Create(nil, 11); //11 more or less arbitrary
  Free := High(UInt16);
  for k := 0 to 4900 - 1 do
  begin
    Ph3Brick702CoordToSymCoord[k].c_idx := Free;
    Ph3Brick702CoordToSymCoord[k].sym := 0;
  end;
  repIdx := 0;
  for k := 0 to 4900 - 1 do
  begin
    if Ph3Brick702CoordToSymCoord[k].c_idx = Free then   //new representant
    begin
      SymCoordRepToPh3Brick702Coord[repIdx] := k; //need this for pruning table

      fc.InvPh3RLFBCenterCoord(k, 2, 3);//2,3 arbitrary

      for i := 0 to 1 do
      begin
        for j := 0 to 3 do
        begin
          sym := 4 * i + j; //0<=sym<8
          coord := fc.Ph3RLFBCenterCoord(2, 3);
          { TODO : die mittelfarben berücksichtigen!! }
          Ph3Brick702CoordToSymCoord[coord].c_idx := repIdx;
          Ph3Brick702CoordToSymCoord[coord].sym :=
            Ph3Brick702CoordToSymCoord[coord].sym or (1 shl sym);
          //apply all 8 symmetries to the representant and fill up to 8 raw coord
          //entries. If  the representant has symmetry itself, we have less entries
          // each containing 2, 4 or 8 sym bits.
          fc.applySymmetry(2, 3, S_U4);
          fc.applySymmetry(5, 5, S_U4); //transform center colors too
        end;
        fc.applySymmetry(2, 3, S_F2);
        fc.applySymmetry(5, 5, S_F2);
      end;
      Inc(repIdx);
    end;
  end;
  fc.Free;
end;



// Phase3RLFBCenterCoordSymTransform[idx,s] gives Phase3RLFBCenterCoord after applying
// the inverse of symmetry s
procedure createPh3RLFBCenterCoordSymTransTable;
var
  fc: faceletCube;
  i, j: integer;
begin
  SetLength(Ph3RLFBCentCoordSymTransform, B_8_4 * B_8_4, 8);
  fc := faceletCube.Create(nil, 11);
  for i := 0 to B_8_4 * B_8_4 - 1 do
  begin
    fc.InvPh3RLFBCenterCoord(i, 2, 3);
    for j := 0 to 7 do
    begin
      fc.applyInvSymmetryByIndex(2, 3, j);
      fc.applyInvSymmetryByIndex(5, 5, j);//also transform centers
      Ph3RLFBCentCoordSymTransform[i, j] := fc.Ph3RLFBCenterCoord(2, 3);
      { TODO : testen! }
      fc.applySymmetryByIndex(2, 3, j); //restore state
      fc.applySymmetryByIndex(5, 5, j);
    end;
  end;
  fc.Free;
end;

//procedure set_bc_depth3(b_class, c: integer; val: UInt32);
//var
//  base, shift, v: UInt32;
//begin
//  base := c shr 4;
//  shift := (c mod 16) * 2;

//  v := Ph3BrickRLFBCentPrun[b_class, base] and (not (3 shl shift));
//  v := v or (val shl shift);
//  Ph3BrickRLFBCentPrun[b_class, base] := v;
//end;

//function get_bc_depth3(b_class, c: integer): integer;
//var
//  base, shift, y: UInt32;
//begin
//  base := c shr 4;
//  shift := (c mod 16) * 2;
//  y := Ph3BrickRLFBCentPrun[b_class, base];
//  Result := (y shr shift) and 3;
//end;


procedure set_bycx_depth3(b_class, c: integer; val: UInt32);
var
  base, shift, v: UInt32;
begin
  base := c shr 4;
  shift := (c mod 16) * 2;

  v := Ph3Brick702RLFBCentPrun[b_class, base] and (not (3 shl shift));
  v := v or (val shl shift);
  Ph3Brick702RLFBCentPrun[b_class, base] := v;
end;

function get_bycx_depth3(b_class, c: integer): integer;
var
  base, shift, y: UInt32;
begin
  base := c shr 4;
  shift := (c mod 16) * 2;
  y := Ph3Brick702RLFBCentPrun[b_class, base];
  Result := (y shr shift) and 3;
end;


procedure createPh3Brick702RLFBCentPruningTable;
//table holds information for brick coordinate and the RLFB-center coordinate
// of the orbits (x,y) and (y,x)
var
  i, j, depth, depth3, bycx, altbycx: UInt32;

  bx, bx_class, bx1, bx1_class, bx1_sym, by, by1, altby, cx, cx1, altcx: UInt16;
  n_chunk: UInt32;
  done, total: UInt64;
  backsearch, match: boolean;
  fs: TFileStream;
  m: Moves;

const
  fName = 'Ph3Brick702RLFBCentPrun';
begin
  n_chunk := 1500625;// B_24_8^4 / 16 //UInt32 contains 16 entries
  total := UInt64(24010000) * N_SYMBRICK702COORD;//70^4*690

  SetLength(Ph3Brick702RLFBCentPrun, N_SYMBRICK702COORD);
  Form1.Memo1.Lines.Add(Format('Initializing 3.9 GB of Memory...', []));
  for i := 0 to N_SYMBRICK702COORD - 1 do
  begin
    Application.ProcessMessages;
    SetLength(Ph3Brick702RLFBCentPrun[i], n_chunk);
  end;

  if FileExists(fName) then
  begin
    Form1.Memo1.Lines.Add(Format('Loading pruning table %s', [fName]));
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := 0 to N_SYMBRICK702COORD - 1 do
    begin
      Application.ProcessMessages;
      fs.ReadBuffer(Ph3Brick702RLFBCentPrun[i][0], n_chunk * SizeOf(UInt32));
    end;
    fs.Free;
    Form1.Memo1.Lines.Add(Format('Done!', []));
  end
  else
  begin
    Form1.Memo1.Lines.Add(Format('Generating pruning table %s', [fName]));
    Form1.Memo1.Lines.Add(Format('This will take several hours.', []));
    for i := 0 to N_SYMBRICK702COORD - 1 do
      for j := 0 to n_chunk - 1 do
        Ph3Brick702RLFBCentPrun[i, j] := $ffffffff;

    bx_class := 0;
    bycx := 0;
    set_bycx_depth3(bx_class, bycx, 0);
    done := 1;
    depth := 0;
    backsearch := False;

    while done <> total do
    begin
      depth3 := depth mod 3;
      if depth = 13 then  //xxx seem appropriate
      begin
        Form1.Memo1.Lines.Add(
          Format('Flipping to backward search at depth %d.', [depth]));
        backsearch := True;
      end;
      for bx_class := 0 to N_SYMBRICK702COORD - 1 do
      begin
        Application.ProcessMessages;

        bycx := 0;
        while bycx < 24010000 do  //70^4
        begin
          if (not backsearch) and (bycx mod 16 = 0) and
            (Ph3Brick702RLFBCentPrun[bx_class, bycx div 16] = $FFFFFFFF) and
            (bycx < 24010000 - 16) then
          begin
            Inc(bycx, 16);
            continue;
          end;
          if backsearch then
            match := (get_bycx_depth3(bx_class, bycx) = 3)
          else
            match := (get_bycx_depth3(bx_class, bycx) = depth3);

          if match then
          begin
            bx := SymCoordRepToPh3Brick702Coord[bx_class];//x brick coord of orbit (x,y)
            by := bycx div 4900;
            //y brick coord of orbit (x,y) = x brick coord of orbit (y,x)
            cx := bycx mod 4900; //bycx:= 4900*by+ cx,cx: center coord of orbit (x,y)
            //cy = center coord of orbit (y,x)is not included in the pruning table

            for m := fR1 to yB3 do //no fU1..fD3 moves
            begin
              if not Phase3Allowed[Ord(m)] then
                continue;
              case m of
                fU1..fB3:
                begin
                  bx1 := Ph3RLFBCenterMove[bx, Ord(m)];
                  cx1 := Ph3RLFBCenterMove[cx, Ord(m)];
                  by1 := Ph3RLFBCenterMove[by, Ord(m)];
                end;
                xU1..xB3:
                begin
                  bx1 := Ph3RLFBCenterMove[bx, Ord(m)];
                  cx1 := Ph3RLFBCenterMove[cx, Ord(m)];
                  by1 := by;// x moves move only x bricks
                end;
                yU1..yB3:
                begin
                  bx1 := bx;// y moves move only y bricks
                  cx1 := Ph3RLFBCenterMove[cx, Ord(m)];
                  by1 := Ph3RLFBCenterMove[by, Ord(m) - 18];
                end;
              end;
              bx1_class := Ph3Brick702CoordToSymCoord[bx1].c_idx;
              bx1_sym := Ph3Brick702CoordToSymCoord[bx1].sym;
              if not backsearch then
              begin
                for i := 0 to 7 do //more than one symmetry possible
                begin
                  if (bx1_sym and (1 shl i)) <> 0 then
                  begin
                    altcx := Ph3RLFBCentCoordSymTransform[cx1, i];
                    altby := Ph3RLFBCentCoordSymTransform[by1, i];
                    altbycx := 4900 * altby + altcx;

                    if get_bycx_depth3(bx1_class, altbycx) = 3 then //entry empty
                    begin
                      set_bycx_depth3(bx1_class, altbycx, (depth + 1) mod 3);
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
                altcx := Ph3RLFBCentCoordSymTransform[cx1, i];
                altby := Ph3RLFBCentCoordSymTransform[by1, i];
                altbycx := 4900 * altby + altcx;

                if get_bycx_depth3(bx1_class, altbycx) = depth3 then //entry set
                begin
                  set_bycx_depth3(bx_class, bycx, (depth + 1) mod 3);
                  Inc(done);
                  break;//no more move m
                end;
              end;

            end;//m
          end;//match

          Inc(bycx);
        end;
      end;//bx_classidx
      Form1.Memo1.Lines.Add(Format('depth: %d, done: %d', [depth, done]));
      Application.ProcessMessages;
      Inc(depth);
      //1,9,96,784,5720,42484,327898,2426711,16325990,96663474,483027267,1920436040,11206360445
    end;
    fs := TFileStream.Create(fName, fmCreate);
    for i := 0 to N_SYMBRICK702COORD - 1 do
      fs.WriteBuffer(Ph3Brick702RLFBCentPrun[i][0], n_chunk * SizeOf(UInt32));
    fs.Free;
    Form1.Memo1.Lines.Add(Format('Pruning table %s created.', [fName]));
  end;
end;




procedure createPh3RLFBPlusCrossPruningTable;
var
  i, depth, done: integer;
  bx, bx1: UInt16;
  mv: Moves;
  // fs: TFileStream;
const
  NSTATES = B_8_4 * B_8_4;
begin
  SetLength(Ph3RLFBPlusCrossPrun, NSTATES);

  for i := 0 to NSTATES - 1 do
    Ph3RLFBPlusCrossPrun[i] := $FF;

  Ph3RLFBPlusCrossPrun[0] := 0;
  done := 1;
  depth := 0;
  while done <> NSTATES do
  begin
    for bx := 0 to 4900 - 1 do // we only move y-bricks
    begin
      if Ph3RLFBPlusCrossPrun[bx] = depth then
      begin
        for mv := fR1 to xB3 do
        begin
          if not Phase3Allowed[Ord(mv)] then
            continue;
          bx1 := Ph3RLFBCenterMove[bx, Ord(mv)];

          if Ph3RLFBPlusCrossPrun[bx1] = $FF then
          begin
            Ph3RLFBPlusCrossPrun[bx1] := depth + 1;
            Inc(done);
          end;
        end;
      end;
    end;
    Inc(depth); //1,7,66,400,1539,3529,4802,4900
    //not more than 7 moves
  end;
end;


procedure createPh3RLFBXCrossPruningTable;
var
  done, depth, i, idx, newidx: integer;
  bx, bx1, cx, cx1: UInt16;
  mv: Moves;
  fs: TFileStream;
const
  NSTATES = 24010000;//B_8_4 * B_8_4*B_8_4 * B_8_4;
  fName = 'Ph3RLFBXCrossPrun';
begin
  SetLength(Ph3RLFBXCrossPrun, NSTATES);
  if FileExists(fName) then
  begin
    fs := TFileStream.Create(fName, fmOpenRead);
    fs.ReadBuffer(Ph3RLFBXCrossPrun[0], NSTATES);
    fs.Free;
  end
  else
  begin
    for i := 0 to NSTATES - 1 do
      Ph3RLFBXCrossPrun[i] := $FF;

    Ph3RLFBXCrossPrun[0] := 0;
    done := 1;
    depth := 0;
    while done <> NSTATES do
    begin
      for bx := 0 to 4900 - 1 do // we only move y-bricks
        for cx := 0 to 4900 - 1 do
        begin
          idx := 4900 * bx + cx;
          if Ph3RLFBXCrossPrun[idx] = depth then
          begin
            for mv := fR1 to xB3 do // no U, D face moves
            begin
              if not Phase3Allowed[Ord(mv)] then
                continue;
              case mv of
                fU1..fB3:
                begin
                  bx1 := Ph3RLFBCenterMove[bx, Ord(mv)];
                  cx1 := Ph3RLFBXCrossMove[cx, Ord(mv)];
                end;
                xU1..xB3:
                begin
                  bx1 := Ph3RLFBCenterMove[bx, Ord(mv)];
                  cx1 := Ph3RLFBXCrossMove[cx, Ord(mv)];
                end;
              end;
              newIdx := 4900 * bx1 + cx1;
              if Ph3RLFBXCrossPrun[newIdx] = $FF then
              begin
                Ph3RLFBXCrossPrun[newIdx] := depth + 1;
                Inc(done);
              end;
            end;
          end;
        end;
      Inc(depth);
      //1,7,74,632,4504,28645,163589,809843,3302952,9893938,18931908,23430308,24006304,24010000
      //not more than 13 moves
    end;
    fs := TFileStream.Create(fName, fmCreate);
    fs.WriteBuffer(Ph3RLFBXCrossPrun[0], NSTATES);
    fs.Free;
  end;
end;



end.
