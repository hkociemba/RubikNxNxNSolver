unit phase2_tables;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cubedefs;

procedure createNextMovePhase2Table;
procedure createFBCenterMoveTable;
procedure createFBFaceMoveAllowedTable;
procedure createFBPlusCrossPruningTable;
procedure createFBFullCenterSliceCoordPruningTable;
procedure createFBSliceMoveTable;
procedure createFBXCrossPruningTable;
procedure createFBXCrossMoveTable;




var
  nextMovePhase2Arr: array [fU1 .. NoMove, InitMove .. yB3] of moves;
  FBCenterMove: array of array of UInt32;
  FBSliceMove: array of array of UInt16;
  FBXCrossMove: array of array of UInt32;
  FBfaceMoveAllowed: array of array of boolean;

  FBPlusCrossPrun: array of byte;
  FBFullCenterSlicePrun: array of byte;
  FBXCrossPrun: array of byte;

implementation

uses facecube;

procedure createNextMovePhase2Table;
var
  fc: faceletCube;
  mprev, mcurr: moves;
begin
  fc := faceletCube.Create(nil, 11); // any odd value possible
  for mcurr := InitMove to yB3 do
    // use NoMove for the predecessor if we have the first move
  begin
    if not Phase2Allowed[Ord(mcurr)] then
      continue;
    nextMovePhase2Arr[NoMove, mcurr] := fc.nextMovePh2(0, mcurr);
  end;
  for mprev := fU1 to yB3 do
  begin
    fc.fxymoves[0] := mprev;
    for mcurr := InitMove to yB3 do
    begin
      if not Phase2Allowed[Ord(mcurr)] then
        continue;
      nextMovePhase2Arr[mprev, mcurr] := fc.nextMovePh2(1, mcurr);
    end;
  end;
  fc.Free;
end;

procedure createFBCenterMoveTable;
var
  i, k: integer;
  a: Axis;
  fs: TFileStream;
  fc: faceletcube;
const
  fName = 'FBCenterMove';
begin
  fc := faceletCube.Create(nil, 11); // 11 arbitrary
  SetLength(FBCenterMove, B_16_8, 3 * 18);
  // some moves are not allowed though
  if FileExists(fName) then
  begin
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := Low(FBCenterMove) to High(FBCenterMove) do
      fs.ReadBuffer(FBCenterMove[i][0], SizeOf(UInt32) * 3 * 18);
    fs.Free;
  end
  else
  begin
    for i := 0 to B_16_8 - 1 do
    begin
      fc.InvPhase2CenterCoord(i, 2, 3);//we take x=2 and y=3
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 0); //face turns
          if (k <> 3) and Phase2Allowed[3 * Ord(a) + k] then
            FBCenterMove[i, 3 * Ord(a) + k] := fc.Phase2CenterCoord(2, 3);
        end;
        for k := 0 to 3 do
        begin
          fc.move(a, 2); //x turns
          if (k <> 3) and Phase2Allowed[3 * Ord(a) + k + 18] then
            FBCenterMove[i, 3 * Ord(a) + k + 18] := fc.Phase2CenterCoord(2, 3);
        end;
        for k := 0 to 3 do
        begin
          fc.move(a, 3); //y turns
          if (k <> 3) and Phase2Allowed[3 * Ord(a) + k + 36] then
            FBCenterMove[i, 3 * Ord(a) + k + 36] := fc.Phase2CenterCoord(2, 3);
        end;
      end;
    end;
    fs := TFileStream.Create(fName, fmCreate);
    for i := Low(FBCenterMove) to High(FBCenterMove) do
      fs.WriteBuffer(FBCenterMove[i][0], SizeOf(UInt32) * 3 * 18);
    fs.Free;
  end;
  fc.Free;
end;


//allow only face moves which do not change the brick16 coordinate
procedure   createFBFaceMoveAllowedTable;
var
  a: Axis;
  slice, pcx, pcy, pcxnew, pcynew, k: integer;
  fc: faceletCube;
begin
  SetLength(FBfaceMoveAllowed, 16, 18); // 16 different values for Phase2Brick16
  for slice := 0 to 15 do
    for k := 0 to 17 do
      FBfaceMoveAllowed[slice, k] := False; // default
  fc := faceletCube.Create(nil, 11);
  for slice := 0 to 15 do
  begin
    fc.InvPhase2SliceCoord(slice, 2, 3);
    pcx := fc.Phase2CenterCoord(fc.size div 2, 2); // (5,2) orbit
    pcy := fc.Phase2CenterCoord(fc.size div 2, 3); // (5,3) orbit
    for a := U to b do
      for k := 0 to 3 do
      begin
        fc.move(a, 0); // facemove
        if k <> 3 then
        begin
          pcxnew := fc.Phase2CenterCoord(fc.size div 2, 2);
          pcynew := fc.Phase2CenterCoord(fc.size div 2, 3);
          if (pcx = pcxnew) and (pcy = pcynew) then
            FBfaceMoveAllowed[slice, 3 * Ord(a) + k] := True
          else
            FBfaceMoveAllowed[slice, 3 * Ord(a) + k] := False;
        end;
      end;
  end;
  fc.Free;
end;

procedure createFBPlusCrossPruningTable;
//we fix the edge parities here, since all following parts do
var
  fc: faceletCube;
  i, idx, newIdx, ep, newep, depth, done: integer;
  mv: Moves;
begin
  SetLength(FBPlusCrossPrun, B_16_8 * 2);
  fc := faceletCube.Create(nil, 11);
  for i := 0 to B_16_8 * 2 - 1 do
    FBPlusCrossPrun[i] := $FF;
  idx := fc.Phase2CenterCoord(2, fc.size div 2); // 2 is arbitrary
  FBPlusCrossPrun[idx] := 0; // distance to solved
  done := 1;
  depth := 0;
  while done <> B_16_8 * 2 do
  begin
    for idx := 0 to B_16_8 - 1 do
      for ep := 0 to 1 do
      begin
        if FBPlusCrossPrun[2*idx+ep] = depth then
        begin
          for mv := fU1 to xB3 do
          begin
            if not Phase2Allowed[Ord(mv)] then
              continue;
            newIdx := FBCenterMove[idx, Ord(mv)];
            if (mv = xU1) or (mv = xU3) or (mv = xD1) or (mv = xD3) then
              newep := 1 - ep
            else
              newep := ep;
            if FBPlusCrossPrun[2*newIdx+newep] = $FF then
            begin
              FBPlusCrossPrun[2*newIdx+newep] := depth + 1;
              Inc(done);
            end;
          end;
        end;
      end;
     Inc(depth); //1,3,28,222,888,2584,6513,10591,12680,12870 without edge parity
    // 1,3,28,222,1058,3860,10815,19252,24715,25730,25740 // with edge parity
    // max 10 moves
  end;
  fc.Free;
end;

procedure createFBFullCenterSliceCoordPruningTable;
{ TODO : mit Symmetrien U2, F2, Spiegelung reduzieren }
var
  depth, idxCent1, idxCent2, idxSlice, newIdxCent1, newIdxCent2, newIdxSlice: UInt32;

  i, done, idx, newIdx: UInt64;
  mv: Moves;
  fs: TFileStream;
const
  fName = 'FBFullCenterSlicePrun';
begin
  SetLength(FBFullCenterSlicePrun, UInt64(16) * B_16_8 * B_16_8);
  if FileExists(fName) then
  begin
    fs := TFileStream.Create(fName, fmOpenRead);
    for i := 0 to 1859 - 1 do // 1859*1425600=16*B_16_8 * B_16_8
      fs.ReadBuffer(FBFullCenterSlicePrun[UInt64(1425600) * i], 1425600);
    fs.Free;
  end
  else
  begin
    for i := 0 to UInt64(16) * B_16_8 * B_16_8 - 1 do
      FBFullCenterSlicePrun[i] := $FF;
    // id value
    FBFullCenterSlicePrun[0] := 0;
    done := 1;
    depth := 0;
    while done <> UInt64(16) * B_16_8 * B_16_8 do
    begin
      for idxSlice := 0 to 16 - 1 do
        for idxCent1 := 0 to B_16_8 - 1 do
          for idxCent2 := 0 to B_16_8 - 1 do
          begin
            idx := UInt64(B_16_8) * (UInt64(B_16_8) * idxSlice +
              idxCent1) + idxCent2;
            if FBFullCenterSlicePrun[idx] = depth then
            begin
              for mv := fU1 to yB3 do
              begin
                case mv of
                  fU1..fB3:
                  begin
                    if not Phase2Allowed[Ord(mv)] or not
                      FBfaceMoveAllowed[idxSlice, Ord(mv)] then
                      continue;
                    newIdxCent1 := FBCenterMove[idxCent1, Ord(mv)];
                    newIdxCent2 := FBCenterMove[idxCent2, Ord(mv)];
                    newIdxSlice := idxSlice;// does not change for allowed moves
                  end;
                  xU1..xB3:
                  begin
                    if not Phase2Allowed[Ord(mv)] then
                      continue;
                    newIdxCent1 := FBCenterMove[idxCent1, Ord(mv)];
                    newIdxCent2 := FBCenterMove[idxCent2, Ord(mv) + 18];
                    newIdxSlice := FBSliceMove[idxSlice, Ord(mv)];
                  end;
                  yU1..yB3:
                  begin
                    if not Phase2Allowed[Ord(mv)] then
                      continue;
                    newIdxCent1 := FBCenterMove[idxCent1, Ord(mv)];
                    newIdxCent2 := FBCenterMove[idxCent2, Ord(mv) - 18];

                    newIdxSlice := FBSliceMove[idxSlice, Ord(mv)];

                  end;

                end;//case
                newIdx := UInt64(B_16_8) * (UInt64(B_16_8) *
                  newIdxSlice + newIdxCent1) + newIdxCent2;
                if FBFullCenterSlicePrun[newIdx] = $FF then
                begin
                  FBFullCenterSlicePrun[newIdx] := depth + 1;
                  Inc(done);
                end;
              end;//mv
            end;
          end;
      Inc(depth);
      //1,5,31,161,958,6043,37107,209637,1191957,6328966,30271598,128467092,446188528,1164176096,2065017528,2551311532,2643832020,2650188028,
    end;
    //1,5,27,135,752,4572,25480,137500,735766,3776206,17568802,74486642,266794020,763200864,1595160960,2341770880,2616625228,2648936348,2650173076,2650190352,2650190400
    fs := TFileStream.Create(fName, fmCreate);
    for i := 0 to 1859 - 1 do
      fs.WriteBuffer(FBFullCenterSlicePrun[UInt64(1425600) * i], 1425600);
    fs.Free;
  end;
end;


procedure createFBXCrossPruningTable;
var
  i, idx, newIdx, done, depth, idxCent, idxSlice, newIdxCent, newIdxSlice: UInt32;
  mv: Moves;
begin
  SetLength(FBXCrossPrun, B_16_8 * 4);

  for i := 0 to B_16_8 * 4 - 1 do
    FBXCrossPrun[i] := $FF;
  // Sollte 0 sein
  FBXCrossPrun[0] := 0; // Abstand zu ID
  done := 1;
  depth := 0;
  while done <> B_16_8 * 4 do
  begin
    for idxSlice := 0 to 4 - 1 do
      for idxCent := 0 to B_16_8 - 1 do
      begin
        idx := B_16_8 * idxSlice + idxCent;
        if FBXCrossPrun[idx] = depth then
        begin
          for mv := fU1 to xB3 do
          begin
            case mv of
              fU1..fB3:
              begin
                if not FBfaceMoveAllowed[idxSlice, Ord(mv)]// or
                then // not Phase2Allowed[Ord(mv)] then
                  continue;
                newIdxCent := FBXCrossMove[idxCent, Ord(mv)];
                newIdxSlice := idxSlice;
              end;
              xU1..xB3:
              begin
                if not Phase2Allowed[Ord(mv)] then
                  continue;
                newIdxCent := FBXCrossMove[idxCent, Ord(mv)];
                newIdxSlice := FBSliceMove[idxSlice, Ord(mv) + 18];
                // UDSliceMoveY, lower bits;

              end;

            end;
            newIdx := B_16_8 * newIdxSlice + newIdxCent;
            if FBXCrossPrun[newIdx] = $FF then
            begin
              FBXCrossPrun[newIdx] := depth + 1;
              Inc(done);
            end;
          end;
        end;
      end;
    Inc(depth);//1,3,14,55,287,1344,5032,15284,34404,48200,51304,51480
    //no more than 11 moves to solve xcross of phase 2
  end;

  // fs := TFileStream.Create(fName, fmCreate);
  // fs.WriteBuffer(FBXCrossPrun[0], B_16_8 * 4);
  // fs.Free;
end;




procedure createFBSliceMoveTable;
var
  i, k: integer;
  a: Axis;
  fc: faceletcube;
begin
  fc := faceletCube.Create(nil, 11); // 11 arbitrary
  SetLength(FBSliceMove, 16, 3 * 18);

  for i := 0 to 15 do
  begin
    fc.InvPhase2SliceCoord(i, 2, 3);//we take e.g. x=2 and y=3
    for a := U to B do
    begin
      for k := 0 to 3 do
      begin
        fc.move(a, 0); //face turns should not change i
        if (k <> 3) and Phase2Allowed[3 * Ord(a) + k] then
          FBSliceMove[i, 3 * Ord(a) + k] := fc.Phase2SliceCoord(2, 3);
      end;
      for k := 0 to 3 do
      begin
        fc.move(a, 2); //x turns
        if (k <> 3) and Phase2Allowed[3 * Ord(a) + k + 18] then
          FBSliceMove[i, 3 * Ord(a) + k + 18] := fc.Phase2SliceCoord(2, 3);
      end;
      for k := 0 to 3 do
      begin
        fc.move(a, 3); //y turns
        if (k <> 3) and Phase2Allowed[3 * Ord(a) + k + 36] then
          FBSliceMove[i, 3 * Ord(a) + k + 36] := fc.Phase2SliceCoord(2, 3);
      end;
    end;
  end;
end;


procedure createFBXCrossMoveTable;
var
  i, k: integer;
  a: Axis;
  fc: faceletcube;
begin
  fc := faceletcube.Create(nil, 11);
  SetLength(FBXCrossMove, B_16_8, 6 * 6);
  for i := 0 to B_16_8 - 1 do
  begin
    fc.InvPhase2CenterCoord(i, 2, 2); //arbitrary
    for a := U to B do
    begin
      for k := 0 to 3 do
      begin
        fc.move(a, 2);
        if (k <> 3) and Phase2Allowed[3 * Ord(a) + k + 18] then
        begin
          FBXCrossMove[i, 3 * Ord(a) + k + 18] := fc.Phase2CenterCoord(2, 2);

        end;
      end;
    end;
  end;
  for i := 0 to B_16_8 - 1 do
  begin
    fc.InvPhase2CenterCoord(i, 2, 2);
    for a := U to B do
    begin
      for k := 0 to 3 do
      begin
        fc.move(a, 0); // face turn
        if (k <> 3) and Phase2Allowed[3 * Ord(a) + k] then
          FBXCrossMove[i, 3 * Ord(a) + k] := fc.Phase2CenterCoord(2, 2);
      end;
    end;
  end;
end;



end.
