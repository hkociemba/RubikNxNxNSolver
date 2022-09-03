unit phase1_tables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cubedefs;

procedure createNextMovePhase1Table;
procedure createUDCenterCoordToSymCoordTable;
procedure createUDCenterCoordSymTransTable;

procedure createUDBrick256CoordSymTransTable;


procedure createUDFaceMoveAllowedTable;
procedure createUDCenterMoveTable;
procedure createUDBrick256MoveTable;
procedure createUDXCrossMoveTable;

procedure createDistanceTable;

function findLowerIndexUDStates10(ccy, Value: integer): integer;
procedure insertUDStates10(ccy, Value: integer);


procedure createUDPlusCross1PruningTable;
procedure createUDCentXBrick256CoordPruningTable;
procedure createUDCentersSlice10;
procedure createUDXCrossPruningTable;

var
  nextMovePhase1: array [fU1 .. NoMove, InitMove .. yB3] of moves;
  UDCentCoordToSymCoord: array of SymCoord32;
  SymCoordRepToUDCenterCoord: array of Int32;

  UDCentCoordSymTransform: array of array of integer;
  UDBrick256CoordSymTransform: array of array of integer;

  UDfaceMoveAllowed: array of array of boolean;

  //movetables for 3 coordinates
  UDCenterMove: array of array of UInt32;
  UDBrick256Move: array of array of UInt16;
  UDXCrossMove: array of array of UInt32;

  UDStates10Table: array of States32;

  UDPlusCross1Prun: array of byte;
  UDCentBrick256Prun: array of byte;
  UDXCrossPrun: array of byte;

  distance: array of integer;
  testCount: array [0 .. 30] of UInt64;

implementation

uses facecube, main, Forms;

procedure createNextMovePhase1Table;
var
  fc: faceletCube;
  mprev, mcurr: moves;
begin
  fc := faceletCube.Create(nil, 11); // any odd value possible
  for mcurr := InitMove to yB3 do
    // use NoMove for the predecessor if we have the first move
    nextMovePhase1[NoMove, mcurr] := fc.nextMovePh1(0, mcurr);

  for mprev := fU1 to yB3 do
  begin
    fc.fxymoves[0] := mprev;
    for mcurr := InitMove to yB3 do
      nextMovePhase1[mprev, mcurr] := fc.nextMovePh1(1, mcurr);
  end;
  fc.Free;
end;

procedure createUDCenterCoordToSymCoordTable;
var
  fc: faceletCube;
var
  i, j, k, sym, repIdx, coord: integer;
  Free: UInt32;
begin
  SetLength(UDCentCoordToSymCoord, B_24_8);
  SetLength(SymCoordRepToUDCenterCoord, N_SYMCENTCOORD);
  // 92247 equivalence classes
  fc := faceletCube.Create(nil, 11); //11 more or less arbitrary
  Free := High(UInt32);
  for k := 0 to B_24_8 - 1 do
  begin
    UDCentCoordToSymCoord[k].c_idx := Free;
    UDCentCoordToSymCoord[k].sym := 0;
  end;
  repIdx := 0;
  for k := 0 to B_24_8 - 1 do
  begin
    if UDCentCoordToSymCoord[k].c_idx = Free then   //new representant
    begin
      SymCoordRepToUDCenterCoord[repIdx] := k;
      // Für Pruning Table Erstellung nötig
      fc.InvPhase1CenterCoord(k, 2, 3); // cluster (2,3) arbitrary
      for i := 0 to 1 do
      begin
        for j := 0 to 3 do
        begin
          sym := 4 * i + j;
          coord := fc.Phase1CenterCoord(2, 3);

          UDCentCoordToSymCoord[coord].c_idx := repIdx;
          UDCentCoordToSymCoord[coord].sym :=
            UDCentCoordToSymCoord[coord].sym or (1 shl sym);
          //apply all 8 symmetries to the representant and fill up to 8 raw coord
          //entries. If  the representant has symmetry itself, we have less entries
          // each containing 2, 4 or 8 sym bits.
          fc.applySymmetry(2, 3, S_U4);
        end;
        fc.applySymmetry(2, 3, S_F2);
      end;
      Inc(repIdx);
    end;
  end;
  fc.Free;
end;



// UDCentCoordSymTransform[idx,s] gives UDCenterCoord after applying
// the inverse of symmetry s
procedure createUDCenterCoordSymTransTable;
var
  fc: faceletCube;
  i, j: integer;
  fs: TFileStream;
const
  fName = 'UDCentTrans';
begin
  SetLength(UDCentCoordSymTransform, B_24_8, 8);
  if FileExists(fName) then
  begin
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := 0 to B_24_8 - 1 do
      fs.ReadBuffer(UDCentCoordSymTransform[i][0], 8 * SizeOf(integer));
    fs.Free;
  end
  else
  begin
    fc := faceletCube.Create(nil, 11);
    for i := 0 to B_24_8 - 1 do
    begin
      fc.InvPhase1CenterCoord(i, 2, 3);
      for j := 0 to 7 do
      begin
        fc.applyInvSymmetryByIndex(2, 3, j);
        UDCentCoordSymTransform[i, j] := fc.Phase1CenterCoord(2, 3);
        fc.applySymmetryByIndex(2, 3, j); //restore state
      end;
    end;
    fc.Free;
    fs := TFileStream.Create(fName, fmCreate);
    for i := 0 to B_24_8 - 1 do
      fs.WriteBuffer(UDCentCoordSymTransform[i][0], 8 * SizeOf(integer));
    fs.Free;
  end;
end;

procedure createUDBrick256CoordSymTransTable;
var
  fc: faceletCube;
  i: integer;
begin
  SetLength(UDBrick256CoordSymTransform, 256, 8);
  fc := faceletCube.Create(nil, 11); // Beim ID-Cube stimmt das UDPlusCross
  for i := 0 to 256 - 1 do
  begin
    fc.InvPhase1Brick256Coord(i, 2, 3);
    UDBrick256CoordSymTransform[i, 0] := fc.Phase1Brick256Coord(2, 3);
    fc.applySymmetry(2, 5, S_F2); // 5=11 div 2
    fc.applySymmetry(3, 5, S_F2);
    UDBrick256CoordSymTransform[i, 4] := fc.Phase1Brick256Coord(2, 3);
    fc.applySymmetry(2, 5, S_F2);
    fc.applySymmetry(3, 5, S_F2);

    fc.applySymmetry(2, 5, S_U4);
    fc.applySymmetry(3, 5, S_U4);
    UDBrick256CoordSymTransform[i, 3] := fc.Phase1Brick256Coord(2, 3);
    fc.applySymmetry(2, 5, S_F2);
    fc.applySymmetry(3, 5, S_F2);
    UDBrick256CoordSymTransform[i, 7] := fc.Phase1Brick256Coord(2, 3);
    fc.applySymmetry(2, 5, S_F2);
    fc.applySymmetry(3, 5, S_F2);

    fc.applySymmetry(2, 5, S_U4);
    fc.applySymmetry(3, 5, S_U4);
    UDBrick256CoordSymTransform[i, 2] := fc.Phase1Brick256Coord(2, 3);
    fc.applySymmetry(2, 5, S_F2);
    fc.applySymmetry(3, 5, S_F2);
    UDBrick256CoordSymTransform[i, 6] := fc.Phase1Brick256Coord(2, 3);
    fc.applySymmetry(2, 5, S_F2);
    fc.applySymmetry(3, 5, S_F2);

    fc.applySymmetry(2, 5, S_U4);
    fc.applySymmetry(3, 5, S_U4);
    UDBrick256CoordSymTransform[i, 1] := fc.Phase1Brick256Coord(2, 3);
    fc.applySymmetry(2, 5, S_F2);
    fc.applySymmetry(3, 5, S_F2);
    UDBrick256CoordSymTransform[i, 5] := fc.Phase1Brick256Coord(2, 3);
  end;
  fc.Free;
end;


procedure createUDFaceMoveAllowedTable;
var
  a: Axis;
  brick, pcx, pcy, pcxnew, pcynew, k: integer;
  fc: faceletCube;
begin
  SetLength(UDfaceMoveAllowed, 256, 18);
  fc := faceletCube.Create(nil, 11);
  for brick := 0 to 255 do
  begin
    fc.InvPhase1Brick256Coord(brick, 2, 3);
    pcx := fc.Phase1CenterCoord(2, 5); //+cross centers
    pcy := fc.Phase1CenterCoord(3, 5);
    for a := U to B do
      for k := 0 to 3 do
      begin
        fc.move(a, 0); // facemove
        if k <> 3 then
        begin
          pcxnew := fc.Phase1CenterCoord(2, 5);
          pcynew := fc.Phase1CenterCoord(3, 5);
          //allowed moves may not change the brick coordinate
          if (pcx = pcxnew) and (pcy = pcynew) then
            UDfaceMoveAllowed[brick, 3 * Ord(a) + k] := True
          else
            UDfaceMoveAllowed[brick, 3 * Ord(a) + k] := False;
        end;
      end;
  end;
  fc.Free;
end;


procedure createUDPlusCross1PruningTable;
var
  fc: faceletCube;
  i, idx, depth, newIdx, done: integer;
  m: Moves;
begin
  SetLength(UDPlusCross1Prun, B_24_8);
  fc := faceletCube.Create(nil, 11);
  for i := 0 to B_24_8 - 1 do
    UDPlusCross1Prun[i] := $FF;
  idx := fc.Phase1CenterCoord(2, fc.size div 2); // Egal ob 2
  UDPlusCross1Prun[idx] := 0; // Abstand zu ID
  done := 1;
  depth := 0;
  while done <> B_24_8 do
  begin
    for idx := 0 to B_24_8 - 1 do
      if UDPlusCross1Prun[idx] = depth then
      begin
        // we use the cluster (x,size div 2)
        for m := fU1 to xB3 do
        begin
          newIdx := UDCenterMove[idx, Ord(m)];
          if UDPlusCross1Prun[newIdx] = $FF then
          begin
            UDPlusCross1Prun[newIdx] := depth + 1;
            Inc(done);
          end;
        end;
      end;
    Inc(depth);  //1,5,59,603,4156,23444,101360,295232,566159,720023,735471
  end;
  fc.Free;
end;

procedure createUDCenterMoveTable;
var
  i, k: integer;
  a: Axis;
  fs: TFileStream;
  fc: faceletcube;
const
  fName = 'UDCenterMove';
begin
  fc := faceletCube.Create(nil, 11); // 11 arbitrary
  SetLength(UDCenterMove, B_24_8, 3 * 18);

  if FileExists(fName) then
  begin
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := Low(UDCenterMove) to High(UDCenterMove) do
      fs.ReadBuffer(UDCenterMove[i][0], SizeOf(UInt32) * 3 * 18);
    fs.Free;
  end
  else
  begin
    for i := 0 to B_24_8 - 1 do
    begin
      fc.InvPhase1CenterCoord(i, 2, 3);//we take x=2 and y=3
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 0); //face turns
          if k <> 3 then
            UDCenterMove[i, 3 * Ord(a) + k] := fc.Phase1CenterCoord(2, 3);
        end;
        for k := 0 to 3 do
        begin
          fc.move(a, 2); //x turns
          if k <> 3 then
            UDCenterMove[i, 3 * Ord(a) + k + 18] := fc.Phase1CenterCoord(2, 3);
        end;
        for k := 0 to 3 do
        begin
          fc.move(a, 3); //y turns
          if k <> 3 then
            UDCenterMove[i, 3 * Ord(a) + k + 36] := fc.Phase1CenterCoord(2, 3);
        end;
      end;
    end;
    fs := TFileStream.Create(fName, fmCreate);
    for i := Low(UDCenterMove) to High(UDCenterMove) do
      fs.WriteBuffer(UDCenterMove[i][0], SizeOf(UInt32) * 3 * 18);
    fs.Free;

  end;
  fc.Free;
end;


procedure createUDBrick256MoveTable;
var
  i, k: integer;
  a: Axis;
  fc: faceletcube;
begin
  fc := faceletCube.Create(nil, 11); // 11 arbitrary
  SetLength(UDBrick256Move, 256, 3 * 18);
  for i := 0 to 256 - 1 do
  begin
    fc.InvPhase1Brick256Coord(i, 2, 3);//we take e.g. x=2 and y=3
    for a := U to B do
    begin
      for k := 0 to 3 do
      begin
        fc.move(a, 0); //face turns should not change i
        if k <> 3 then
          UDBrick256Move[i, 3 * Ord(a) + k] := fc.Phase1Brick256Coord(2, 3);
      end;
      for k := 0 to 3 do
      begin
        fc.move(a, 2); //x turns
        if k <> 3 then
          UDBrick256Move[i, 3 * Ord(a) + k + 18] := fc.Phase1Brick256Coord(2, 3);
      end;
      for k := 0 to 3 do
      begin
        fc.move(a, 3); //y turns
        if k <> 3 then
          UDBrick256Move[i, 3 * Ord(a) + k + 36] := fc.Phase1Brick256Coord(2, 3);
      end;
    end;
  end;
  fc.Free;
end;




//array distance computes the new distance from the old_distance i and the new_distance_mod3 j.
procedure createDistanceTable;
var
  i, j: integer;
begin
  setLength(distance, 25 * 3);
  for i := 0 to 24 do
    for j := 0 to 2 do
    begin
      distance[3 * i + j] := (i div 3) * 3 + j;
      if (i mod 3 = 2) and (j = 0) then
        Inc(distance[3 * i + j], 3)
      else if (i mod 3 = 0) and (j = 2) then
        Dec(distance[3 * i + j], 3);
    end;
end;


function findLowerIndexUDStates10(ccy, Value: integer): integer;
var
  left, right, m: integer;
begin
  right := UDStates10Table[ccy].used - 1;

  if right = -1 then
    Exit(0); // an Position 0 einfügen
  if UDStates10Table[ccy].state[0] > Value then
    Exit(0);
  if UDStates10Table[ccy].state[0] = Value then
    Exit(-1);
  if UDStates10Table[ccy].state[right] < Value then
    Exit(right + 1);
  if UDStates10Table[ccy].state[right] = Value then
    Exit(-1);
  left := 0;
  while right - left > 1 do
  begin
    m := (right + left) div 2;
    if UDStates10Table[ccy].state[m] < Value then
      left := m
    else if UDStates10Table[ccy].state[m] > Value then
      right := m
    else
      Exit(-1); // überhaupt nicht einfügen
  end;

  // wenn right-left=1 sind weder right noch left Treffer, da left kleiner und right größer
  Result := right;
end;

// fügt neuen Wert in sortierten Array ein, falls er noch nicht existiert
procedure insertUDStates10(ccy, Value: integer);
var
  ins, i, used: integer;
begin
  ins := findLowerIndexUDStates10(ccy, Value);
  if ins = -1 then
    Exit;
  used := UDStates10Table[ccy].used;
  if Length(UDStates10Table[ccy].state) = used then
  begin
    SetLength(UDStates10Table[ccy].state, 2 * used);
    // for j := used to 2 * used - 1 do
    // UDStates10Table[ccy].state[j] := MaxInt;
  end;
  for i := used - 1 downto ins do // Platz machen
    UDStates10Table[ccy].state[i + 1] := UDStates10Table[ccy].state[i];
  UDStates10Table[ccy].state[ins] := Value;
  Inc(UDStates10Table[ccy].used);

  Inc(testCount[0]);
end;


procedure createUDCentXBrick256CoordPruningTable;
var
  i, idx, idx1, done, depth, cx, bx, cx1, bx1: UInt32;
  m: Moves;
  fs: TFileStream;
const
  fName = 'UDCentBrick256Prun';
begin
  SetLength(UDCentBrick256Prun, B_24_8 * 256);
  if FileExists(fName) then
  begin
    fs := TFileStream.Create(fName, fmOpenRead);
    fs.ReadBuffer(UDCentBrick256Prun[0], B_24_8 * 256);
    fs.Free;
  end
  else
  begin
    for i := 0 to B_24_8 * 256 - 1 do
      UDCentBrick256Prun[i] := $FF;
    // Sollte 0 sein
    UDCentBrick256Prun[0] := 0; // Abstand zu ID
    done := 1;
    depth := 0;
    while done <> B_24_8 * 256 do
    begin
      for bx := 0 to 256 - 1 do
        for cx := 0 to B_24_8 - 1 do
        begin
          idx := B_24_8 * bx + cx;
          if UDCentBrick256Prun[idx] = depth then
          begin
            for m := fU1 to yB3 do
            begin
              if (m < xU1) and (not UDfaceMoveAllowed[bx, Ord(m)]) then
                continue;
              bx1 := UDBrick256Move[bx, Ord(m)];
              cx1 := UDCenterMove[cx, Ord(m)];
              idx1 := B_24_8 * bx1 + cx1;
              if UDCentBrick256Prun[idx1] = $FF then
              begin
                UDCentBrick256Prun[idx1] := depth + 1;
                Inc(done);
              end;
            end;

          end;
        end;
      Inc(depth);
      //1,9,125,1533,14942,116505,812533,4655525,20599393,64766073,133354513,178736089,187934956,188280128,188280576
    end;
    fs := TFileStream.Create(fName, fmCreate);
    fs.WriteBuffer(UDCentBrick256Prun[0], B_24_8 * 256);
    fs.Free;
  end;
end;


procedure createUDCentersSlice10;
var
  i, j, k, n: integer;
  ccx, ccy, slice, newccx, newccy, altnewccy, newslice, altnewslice: integer;
  mv: Moves;
  fs: TFileStream;
  used: integer;
  sc1: SymCoord32;
  syms: UInt8;
  savearr: array of States32;

const
  fName = 'UDCentersSlice10';

begin
  SetLength(UDStates10Table, N_SYMCENTCOORD * 256);
  if FileExists(fName) then
  begin
    Form1.Memo1.Lines.Add('Loading ' + fName);
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := 0 to N_SYMCENTCOORD * 256 - 1 do
    begin
      if i mod 180000 = 0 then
        Form1.memo1.Lines.Text := Form1.memo1.Lines.Text + '.';
      Application.ProcessMessages;
      fs.ReadBuffer(used, SizeOf(used));
      UDStates10Table[i].used := used;
      SetLength(UDStates10Table[i].state, used);
      if used > 0 then
        fs.ReadBuffer(UDStates10Table[i].state[0], used * 4);
    end;
    fs.Free;
    Form1.Memo1.Lines.Add('Finished loading ' + fName);
  end
  else
  begin
    Form1.Memo1.Lines.Add('Creating ' + fName);
    Form1.Memo1.Lines.Add('This will take several hours.');

    // für alle reduzierten (ccx,slice)
    SetLength(savearr, N_SYMCENTCOORD * 256); // Kopie
    for i := 0 to N_SYMCENTCOORD * 256 - 1 do
    begin
      SetLength(UDStates10Table[i].state, 20);
      // Im Schnitt werden es ca. 140 bei Tiefe 11
      UDStates10Table[i].used := 0;
    end;
    UDStates10Table[0].state[0] := 0;
    Inc(UDStates10Table[0].used);
    testCount[0] := 1;

    for k := 1 to 10 do
    begin
      for i := 0 to N_SYMCENTCOORD * 256 - 1 do
      begin
        used := UDStates10Table[i].used;
        SetLength(savearr[i].state, used);
        savearr[i].used := used;
        for j := 0 to used - 1 do
          savearr[i].state[j] := UDStates10Table[i].state[j];
      end;
      for i := 0 to N_SYMCENTCOORD * 256 - 1 do
      begin
        Application.ProcessMessages;
        used := savearr[i].used;
        slice := i mod 256;
        ccx := SymCoordRepToUDCenterCoord[i div 256];
        for j := 0 to used - 1 do
        begin
          ccy := savearr[i].state[j];
          // ccx enthält die UDCenterCoord des Repäsentanten der Klasse
          // ccy und slice sind ja schon transformiert worden


          for mv := fU1 to yB3 do
          begin
            case mv of
              fU1..fB3:
              begin
                if (not UDfaceMoveAllowed[slice, Ord(mv)]) then
                  continue;
                newccx := UDCenterMove[ccx, Ord(mv)];
                newccy := UDCenterMove[ccy, Ord(mv)];
                newslice := slice;
              end;
              xU1..xB3:
              begin
                newccx := UDCenterMove[ccx, Ord(mv)];
                newccy := UDCenterMove[ccy, Ord(mv) + 18];
                newslice := UDBrick256Move[slice, Ord(mv)];
              end;
              yU1..yB3:
              begin
                newccx := UDCenterMove[ccx, Ord(mv)];
                newccy := UDCenterMove[ccy, Ord(mv) - 18];
                newslice := UDBrick256Move[slice, Ord(mv)];
              end;
            end;

            sc1 := UDCentCoordToSymCoord[newccx]; // Sym-Koordinate
            syms := sc1.sym;

            for n := 0 to 7 do // Für alle möglichen Symmetrien
            begin
              if (syms and (1 shl n)) <> 0 then
              begin
                altnewccy := UDCentCoordSymTransform[newccy, n];
                altnewslice := UDBrick256CoordSymTransform[newslice, n];
                // sollte auch 0 sein
              end;
            end;
            insertUDStates10((sc1.c_idx shl 8) + altnewslice, altnewccy);
          end;//mv
        end;//j
      end; //i
      Form1.Memo1.Lines.Add(IntToStr(testCount[0]));
    end;//k

    Form1.Memo1.Lines.Add('Writing ' + fName);
    fs := TFileStream.Create(fName, fmCreate);
    for i := 0 to N_SYMCENTCOORD * 256 - 1 do
    begin
      used := UDStates10Table[i].used;
      SetLength(UDStates10Table[i].state, used);
      SetLength(savearr[i].state, 0);
      fs.WriteBuffer(used, SizeOf(used));
      if used > 0 then
        fs.WriteBuffer(UDStates10Table[i].state[0], used * 4);
    end;
    fs.Free;
    Form1.Memo1.Lines.Add('Finished writing ' + fName);
  end;
  SetLength(savearr, 0);
end;

procedure createUDXCrossMoveTable;
var
  i, k: integer;
  a: Axis;
  fc: faceletcube;
  fs: TFileStream;
const
  fName = 'UDXCrossMove';
begin
  SetLength(UDXCrossMove, B_24_8, 6 * 6);
  if FileExists(fName) then
  begin
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := Low(UDXCrossMove) to High(UDXCrossMove) do
      fs.ReadBuffer(UDXCrossMove[i][0], SizeOf(UInt32) * 6 * 6);
    fs.Free;
  end
  else
  begin
    fc := faceletCube.Create(nil, 11);
    for i := 0 to B_24_8 - 1 do
    begin
      fc.InvPhase1CenterCoord(i, 2, 2);//2 is arbitrary
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 0); //face turns
          if k <> 3 then
            UDXCrossMove[i, 3 * Ord(a) + k] := fc.Phase1CenterCoord(2, 2);
        end;
        for k := 0 to 3 do
        begin
          fc.move(a, 2); //x=2=y turns
          if k <> 3 then
            UDXCrossMove[i, 3 * Ord(a) + k + 18] := fc.Phase1CenterCoord(2, 2);
        end;
      end;
    end;
    fs := TFileStream.Create(fName, fmCreate);
    for i := Low(UDXCrossMove) to High(UDXCrossMove) do
      fs.WriteBuffer(UDXCrossMove[i][0], SizeOf(UInt32) * 6 * 6);
    fs.Free;
  end;
end;


procedure createUDXCrossPruningTable;
var
  i, idx, newIdx, done, depth, cx, by, cx1, by1: UInt32;
  fs: TFileStream;
  mv: Moves;
const
  fName = 'UDXCrossPrun';
begin
  SetLength(UDXCrossPrun, B_24_8 * 16);
  if FileExists(fName) then
  begin
    fs := TFileStream.Create(fName, fmOpenRead);
    fs.ReadBuffer(UDXCrossPrun[0], B_24_8 * 16);
    fs.Free;
  end
  else
  begin
    for i := 0 to B_24_8 * 16 - 1 do
      UDXCrossPrun[i] := $FF;

    UDXCrossPrun[0] := 0;
    done := 1;
    depth := 0;
    while done <> B_24_8 * 16 do
    begin
      for by := 0 to 16 - 1 do // we only move y-bricks
        for cx := 0 to B_24_8 - 1 do
        begin
          idx := B_24_8 * by + cx;
          if UDXCrossPrun[idx] = depth then
          begin
            for mv := fU1 to xB3 do
            begin
              case mv of
                fU1..fB3:
                begin
                  if (not UDfaceMoveAllowed[by, Ord(mv)]) then
                    continue;
                  cx1 := UDXCrossMove[cx, Ord(mv)];
                  by1 := by;
                end;
                xU1..xB3:
                begin
                  cx1 := UDXCrossMove[cx, Ord(mv)];
                  by1 := UDBrick256Move[by, Ord(mv) + 18];//y-bricks in lower bits!
                end;
              end;
              newIdx := B_24_8 * by1 + cx1;
              if UDXCrossPrun[newIdx] = $FF then
              begin
                UDXCrossPrun[newIdx] := depth + 1;
                Inc(done);
              end;
            end;
          end;
        end;
      Inc(depth);
      //1,5,55,543,4015,25947,166128,829860,3243198,8166274,11535582,11767488,11767536
    end;
    fs := TFileStream.Create(fName, fmCreate);
    fs.WriteBuffer(UDXCrossPrun[0], B_24_8 * 16);
    fs.Free;
  end;
end;


end.
























