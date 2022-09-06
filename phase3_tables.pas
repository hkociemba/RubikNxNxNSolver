unit phase3_tables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cubedefs;

procedure createNextMovePhase3Table;
procedure createPhase3CenterMoveTable;
procedure createPhase3Brick4096MoveTable;
procedure createPh3FaceMoveAllowedTable;
procedure createPhase3RLFBCenterMoveTable;
procedure createPh3Brick4096CoordToSymCoordTable;
procedure createPh3RLFBCenterCoordSymTransTable;
procedure createPh3Brick4096RLFBCentPruningTable;

var
  Ph3faceMoveAllowed: array of array of boolean;
  nextMovePhase3Arr: array [fU1 .. NoMove, InitMove .. yB3] of moves;
  Phase3CenterMove: array of array of array of UInt32;
  Phase3RLFBCenterMove: array of array of UInt16;
  Ph3Brick4096Move: array of array of UInt16;

  Ph3Brick4096CoordToSymCoord: array of SymCoord16;
  SymCoordRepToPh3Brick4096Coord: array of UInt16;

  Ph3RLFBCentCoordSymTransform: array of array of UInt16;
  Ph3BrickRLFBCentPrun:array of array of UInt32;


implementation

uses facecube, main, forms;

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

procedure createPhase3RLFBCenterMoveTable;
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
  SetLength(Phase3RLFBCenterMove, B_8_4 * B_8_4, 3 * 18);

  if FileExists(fName) then
  begin
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := Low(Phase3RLFBCenterMove) to High(Phase3RLFBCenterMove) do
      fs.ReadBuffer(Phase3RLFBCenterMove[i][0], SizeOf(UInt16) * 3 * 18);
    fs.Free;
  end
  else
  begin
    n := High(UInt16);
    for i := 0 to B_8_4 * B_8_4 - 1 do
      for k := 0 to 53 do
        Phase3RLFBCenterMove[i, k] := n;

    for i := 0 to B_8_4 * B_8_4 - 1 do
    begin
      fc.InvPhase3RLFBCenterCoord(i, 2, 3);//2, 3 arbitrary
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 0); // Faceturns
          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k] then
            Phase3RLFBCenterMove[i, 3 * Ord(a) + k] :=
              fc.Phase3RLFBCenterCoord(2, 3);
        end;
      end;
    end;

    for i := 0 to B_8_4 * B_8_4 - 1 do
    begin
      fc.InvPhase3RLFBCenterCoord(i, 2, 3); // U,R,F: 0,2,4
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 2); //x moves
          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k + 18] then
            Phase3RLFBCenterMove[i, 3 * Ord(a) + k + 18] :=
              fc.Phase3RLFBCenterCoord(2, 3);
        end;
      end;
    end;

    for i := 0 to B_8_4 * B_8_4 - 1 do
    begin
      fc.InvPhase3RLFBCenterCoord(i, 2, 3);
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 3);//y moves
          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k + 36] then
            Phase3RLFBCenterMove[i, 3 * Ord(a) + k + 36] :=
              fc.Phase3RLFBCenterCoord(2, 3);
        end;
      end;
    end;
    fs := TFileStream.Create(fName, fmCreate);
    for i := Low(Phase3RLFBCenterMove) to High(Phase3RLFBCenterMove) do
      fs.WriteBuffer(Phase3RLFBCenterMove[i][0], SizeOf(UInt16) * 3 * 18);
    fs.Free;
  end;
  fc.Free;
end;


procedure createPhase3CenterMoveTable;
var
  i, j, k: integer;
  a: Axis;
  n: UInt32;
  fc: faceletcube;
begin
  fc := faceletCube.Create(nil, 11); // 11 arbitrary
  n := High(UInt32);
  SetLength(Phase3CenterMove, 3, B_8_4, 3 * 18);
  //first coordinate direction 0,1,2 for UD RL FB
  // einige moves are not allowed


  for j := 0 to 2 do
    for i := 0 to B_8_4 - 1 do
      for k := 0 to 53 do
        Phase3CenterMove[j, i, k] := n;


  for j := 0 to 2 do
    for i := 0 to B_8_4 - 1 do
    begin
      fc.InvPhase3CenterCoord(i, 2, 3, Axis(2 * j));//2, 3 arbitrary
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 0); // Faceturns
          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k] then
            Phase3CenterMove[j, i, 3 * Ord(a) + k] :=
              fc.Phase3CenterCoord(2, 3, Axis(2 * j));
        end;
      end;
    end;


  for j := 0 to 2 do
    for i := 0 to B_8_4 - 1 do
    begin
      fc.InvPhase3CenterCoord(i, 2, 3, Axis(2 * j)); // U,R,F: 0,2,4
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 2); //x moves
          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k + 18] then
            Phase3CenterMove[j, i, 3 * Ord(a) + k + 18] :=
              fc.Phase3CenterCoord(2, 3, Axis(2 * j));
        end;
      end;
    end;

  for j := 0 to 2 do
    for i := 0 to B_8_4 - 1 do
    begin
      fc.InvPhase3CenterCoord(i, 2, 3, Axis(2 * j));
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 3);//y moves
          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k + 36] then
            Phase3CenterMove[j, i, 3 * Ord(a) + k + 36] :=
              fc.Phase3CenterCoord(2, 3, Axis(2 * j));
        end;
      end;
    end;

  fc.Free;
end;

procedure createPhase3Brick4096MoveTable;
var
  i, j, k: integer;
  a: Axis;
  n: UInt16;
  fs: TFileStream;
  fc: faceletcube;
const
  fName = 'Ph3Brick4096Move';
begin
  fc := faceletcube.Create(nil, 11);//11 arbitrary
  SetLength(Ph3Brick4096Move, 4096, 3 * 18);

  if FileExists(fName) then
  begin
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := Low(Ph3Brick4096Move) to High(Ph3Brick4096Move) do
      fs.ReadBuffer(Ph3Brick4096Move[i][0], SizeOf(UInt16) * 3 * 18);
    fs.Free;
  end
  else
  begin
    n := High(UInt16);

    for i := 0 to 4095 do
      for j := 0 to 53 do
        Ph3Brick4096Move[i, j] := n;

    for i := 0 to 4096 - 1 do
    begin
      fc.InvPhase3Brick4096Coord(i, 2, 3);
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 0);
          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k] then
            Ph3Brick4096Move[i, 3 * Ord(a) + k] :=
              fc.Phase3Brick4096Coord(2, 3);
          //U and D moves do not change the coordinate! This is correct
          //since we do not care about the U and D centers here.
        end;
      end;
    end;

    for i := 0 to 4096 - 1 do
    begin
      fc.InvPhase3Brick4096Coord(i, 2, 3);
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 2); //x move
          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k + 18] then
            Ph3Brick4096Move[i, 3 * Ord(a) + k + 18] :=
              fc.Phase3Brick4096Coord(2, 3);
        end;
      end;
    end;

    for i := 0 to 4096 - 1 do
    begin
      fc.InvPhase3Brick4096Coord(i, 2, 3);
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 3); //y move
          if (k <> 3) and Phase3Allowed[3 * Ord(a) + k + 36] then
            Ph3Brick4096Move[i, 3 * Ord(a) + k + 36] :=
              fc.Phase3Brick4096Coord(2, 3);
        end;
      end;
    end;
    fs := TFileStream.Create(fName, fmCreate);
    for i := Low(Ph3Brick4096Move) to High(Ph3Brick4096Move) do
      fs.WriteBuffer(Ph3Brick4096Move[i][0], SizeOf(UInt16) * 3 * 18);
    fs.Free;
  end;
  fc.Free;
end;


//allow only face moves which do not change the Ph3Brick4096 coordinate
procedure createPh3FaceMoveAllowedTable;
var
  a: Axis;
  br, pcx, pcy, pcxnew, pcynew, i, k: integer;
  fs: TFileStream;
  fc: faceletCube;
const
  fname = 'ph3fma';
begin
  SetLength(Ph3faceMoveAllowed, 4096, 18); // 4096 different values for Ph3Brick4096
  fc := faceletCube.Create(nil, 11);

  if FileExists(fName) then
  begin
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := Low(Ph3faceMoveAllowed) to High(Ph3faceMoveAllowed) do
      fs.ReadBuffer(Ph3faceMoveAllowed[i][0], SizeOf(boolean) * 18);
    fs.Free;
  end
  else
  begin
    for br := 0 to 4095 do
      for k := 0 to 17 do
        Ph3faceMoveAllowed[br, k] := False; // default

    for br := 0 to 4095 do
    begin
      fc.InvPhase3Brick4096Coord(br, 2, 3);
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 0); // facemove
          if k <> 3 then
          begin
            if fc.Phase3Brick4096Coord(2, 3)= br then //U and D moves do not change the coordinate!
            begin
              Ph3faceMoveAllowed[br, 3 * Ord(a) + k] := True;
            end
            else
              Ph3faceMoveAllowed[br, 3 * Ord(a) + k] := False;
          end;
        end;
      end;//k
    end;
    fs := TFileStream.Create(fName, fmCreate);
    for i := Low(Ph3faceMoveAllowed) to High(Ph3faceMoveAllowed) do
      fs.WriteBuffer(Ph3faceMoveAllowed[i][0], SizeOf(boolean) * 18);
    fs.Free;
  end;
  fc.Free;
end;

procedure createPh3Brick4096CoordToSymCoordTable;
//The bricks have 48-fold symmetry, we use only 8 symmetries
var
  fc: faceletCube;
var
  i, j, k, sym, repIdx, coord: integer;
  Free: UInt16;
begin
  SetLength(Ph3Brick4096CoordToSymCoord, 4096);
  SetLength(SymCoordRepToPh3Brick4096Coord, 640);{ TODO : anpassen }
  // 640 equivalence classes
  fc := faceletCube.Create(nil, 11); //11 more or less arbitrary
  Free := High(UInt16);
  for k := 0 to 4096 - 1 do
  begin
    Ph3Brick4096CoordToSymCoord[k].c_idx := Free;
    Ph3Brick4096CoordToSymCoord[k].sym := 0;
  end;
  repIdx := 0;
  for k := 0 to 4096 - 1 do
  begin
    if Ph3Brick4096CoordToSymCoord[k].c_idx = Free then   //new representant
    begin
      SymCoordRepToPh3Brick4096Coord[repIdx] := k;
      // Für Pruning Table Erstellung nötig
      fc.InvPhase3Brick4096Coord(k, 2, 3);//2,3 arbitrary

      for i := 0 to 1 do
      begin
        for j := 0 to 3 do
        begin
          sym := 4 * i + j; //0<=sym<8
          coord := fc.Phase3Brick4096Coord(2, 3);
          { TODO : die mittelfarben berücksichtigen!! }
          Ph3Brick4096CoordToSymCoord[coord].c_idx := repIdx;
          Ph3Brick4096CoordToSymCoord[coord].sym :=
            Ph3Brick4096CoordToSymCoord[coord].sym or (1 shl sym);
          //apply all 16 symmetries to the representant and fill up to 16 raw coord
          //entries. If  the representant has symmetry itself, we have less entries
          // each containing 2, 4, 8 or 16 sym bits.
          fc.applySymmetry(2, 5, S_U4);
          fc.applySymmetry(3, 5, S_U4);
          fc.applySymmetry(5, 5, S_U4); //transform center colors too
        end;
        fc.applySymmetry(2, 5, S_F2);
        fc.applySymmetry(3, 5, S_F2);
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
    fc.InvPhase3RLFBCenterCoord(i, 2, 3);
    for j := 0 to 7 do
    begin
      fc.applyInvSymmetryByIndex(2, 3, j);
      fc.applyInvSymmetryByIndex(5, 5, j);//also transform centers
      Ph3RLFBCentCoordSymTransform[i, j] := fc.Phase3RLFBCenterCoord(2, 3); { TODO : testen! }
      fc.applySymmetryByIndex(2, 3, j); //restore state
      fc.applySymmetryByIndex(5, 5, j);
    end;
  end;
  fc.Free;
end;

procedure set_bc_depth3(b_class, c: integer; val: UInt32);
var
  base, shift, v: UInt32;
begin
  base := c shr 4;
  shift := (c mod 16) * 2;

  v := Ph3BrickRLFBCentPrun[b_class, base] and (not (3 shl shift));
  v := v or (val shl shift);
  Ph3BrickRLFBCentPrun[b_class, base] := v;
end;

function get_bc_depth3(b_class, c: integer): integer;
var
  base, shift, y: UInt32;
begin
  base := c shr 4;
  shift := (c mod 16) * 2;
  y := Ph3BrickRLFBCentPrun[b_class, base];
  Result := (y shr shift) and 3;
end;



procedure createPh3Brick4096RLFBCentPruningTable;
//table holds information for brick4096 coordinate and the RLFB-center coordinate
// of the orbits (x,y) and (y,x)
var
  i, j, depth,depth3,cxy,altcxy1: UInt32;

  brick,brick_class, brick_sym, brick1,brick1_class, brick1_sym,cx,cy,cx1,cy1,altcx1,altcy1: UInt16;
  n_chunk:UInt32;
  done, total: UInt64;
  backsearch, match: boolean;
  fs: TFileStream;
  m: Moves;

const
  fName = 'Ph3BrickRLFBCentPrun';
begin
  n_chunk := 1500625;// B_24_8^4 / 16
  total := UInt64(24010000) * N_SYMBRICK4096COORD;//70^4*640

  SetLength(Ph3BrickRLFBCentPrun, N_SYMBRICK4096COORD);
  Form1.Memo1.Lines.Add(Format('Initializing 3.6 GB of Memory...', []));
  for i := 0 to N_SYMBRICK4096COORD - 1 do
  begin
    Application.ProcessMessages;
    SetLength(Ph3BrickRLFBCentPrun[i], n_chunk);
  end;

  if FileExists(fName) then
  begin
    Form1.Memo1.Lines.Add(Format('Loading pruning table %s', [fName]));
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := 0 to N_SYMBRICK4096COORD - 1 do
    begin
      Application.ProcessMessages;
      fs.ReadBuffer(Ph3BrickRLFBCentPrun[i][0], n_chunk * SizeOf(UInt32));
    end;
    fs.Free;
    Form1.Memo1.Lines.Add(Format('Done!', []));
  end
  else
  begin
    Form1.Memo1.Lines.Add(Format('Generating pruning table %s', [fName]));
    Form1.Memo1.Lines.Add(Format('This will take several hours.', []));
    for i := 0 to N_SYMBRICK4096COORD - 1 do
      for j := 0 to n_chunk - 1 do
        Ph3BrickRLFBCentPrun[i, j] := $ffffffff;

    brick_class := 0;
    cxy := 0;
    set_bc_depth3(brick_class, cxy, 0);
    done := 1;
    depth := 0;
    backsearch := False;

    while done <> total do
    begin
      depth3 := depth mod 3;
      if depth = 100 then  //12 seem appropriate
      begin
        Form1.Memo1.Lines.Add(
          Format('Flipping to backward search at depth %d.', [depth]));
        backsearch := True;
      end;
      for brick_class := 0 to N_SYMBRICK4096COORD - 1 do
      begin
        Application.ProcessMessages;

        cxy := 0;
        while cxy < 24010000 do  //70^4
        begin
          if (not backsearch) and (cxy mod 16 = 0) and
            (Ph3BrickRLFBCentPrun[brick_class, cxy div 16] = $FFFFFFFF) and
            (cxy < 24010000 - 16) then
          begin
            Inc(cxy, 16);
            continue;
          end;
          if backsearch then
            match := (get_bc_depth3(brick_class, cxy) = 3)
          else
            match := (get_bc_depth3(brick_class, cxy) = depth3);

          if match then
          begin
            brick := SymCoordRepToPh3Brick4096Coord[brick_class];
            cx:= cxy div 4900;
            cy:= cxy mod 4900;

            for m := fU1 to yB3 do   { TODO : überlegen ob man U-D moves nicht erlauben sollte }
            begin
              if not Phase3Allowed[Ord(m)] then continue;
              case m of
                fU1..fB3:
                begin
                  if (m>=fR1) and not Ph3faceMoveAllowed[brick,Ord(m)] then continue;
                  brick1:=  Ph3Brick4096Move[brick,Ord(m)];
                  cx1 := Phase3RLFBCenterMove[cx, Ord(m)];
                  cy1 := Phase3RLFBCenterMove[cy, Ord(m)];
                end;
                xU1..xB3:
                begin
                  brick1:=  Ph3Brick4096Move[brick,Ord(m)];
                  cx1 := Phase3RLFBCenterMove[cx, Ord(m)];
                  cy1 := Phase3RLFBCenterMove[cy, Ord(m)+18];
                end;
                yU1..yB3:
                begin//y-moves move only (x,y)-orbit centers
                  brick1:=  Ph3Brick4096Move[brick,Ord(m)];
                  cx1 := Phase3RLFBCenterMove[cx, Ord(m)];
                  cy1 := Phase3RLFBCenterMove[cy, Ord(m)-18];
                end;
              end;
              brick1_class := Ph3Brick4096CoordToSymCoord[brick1].c_idx;
              brick1_sym := Ph3Brick4096CoordToSymCoord[brick1].sym;//contains up to 8 symmetries
              //cxy1:= B_24_8*B_24_8 * cx1 + cy1;
              //cx1 center coord of orbit (x,y), cy1 center coord of (y,x)
              if not backsearch then
              begin
                for i := 0 to 7 do //more than one symmetry possible
                begin
                  if (brick1_sym and (1 shl i)) <> 0 then
                  begin
                    altcx1:=Ph3RLFBCentCoordSymTransform[cx1,i];
                    altcy1:=Ph3RLFBCentCoordSymTransform[cy1,i];
                    altcxy1:=4900 * altcx1 + altcy1;

                    if get_bc_depth3(brick1_class, altcxy1) = 3 then //entry empty
                    begin
                     set_bc_depth3(brick1_class, altcxy1, (depth + 1) mod 3);
                      Inc(done);
                      //Form1.Memo1.Lines.Add(Format('classidx: %d, by: %d, done: %d', [bx1_class,altby1,done]));
                    end;
                  end;
                end;
              end
              else //backwards search
              begin
                i := 0;//find one symmetry
                while (brick1_sym and (1 shl i)) = 0 do
                  Inc(i);
                altcx1:=Ph3RLFBCentCoordSymTransform[cx1,i];
                altcy1:=Ph3RLFBCentCoordSymTransform[cy1,i];
                altcxy1:=4900 * altcx1 + altcy1;

               if get_bc_depth3(brick1_class, altcxy1) = depth3 then //entry set
                begin
                  set_bc_depth3(brick_class, cxy, (depth + 1) mod 3);
                  Inc(done);
                  break;//no more move m
                end;
              end;

            end;//m
          end;//match

          Inc(cxy);
        end;
      end;//bx_classidx
      Form1.Memo1.Lines.Add(Format('depth: %d, done: %d', [depth, done]));
      Application.ProcessMessages;
      Inc(depth);
    end;
    fs := TFileStream.Create(fName, fmCreate);
    for i := 0 to N_SYMBRICK4096COORD - 1 do
      fs.WriteBuffer(Ph3BrickRLFBCentPrun[i][0], n_chunk * SizeOf(UInt32));
    fs.Free;
  end;
end;



end.




