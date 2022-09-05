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

var
  nextMovePhase3Arr: array [fU1 .. NoMove, InitMove .. yB3] of moves;
  Phase3CenterMove: array of array of array of UInt32;
  Phase3RLFBCenterMove: array of array of UInt16;
  Ph3Brick4096Move: array of array of UInt16;
  Ph3faceMoveAllowed: array of array of boolean;


implementation

uses facecube;

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
  n: UInt32;
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
    n := High(UInt32);
    for i := 0 to 4900 - 1 do
      for k := 0 to 53 do
        Phase3RLFBCenterMove[i, k] := n;

    for i := 0 to 4900 - 1 do
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


    for i := 0 to 4900 - 1 do
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

    for i := 0 to 4900 - 1 do
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
          //some of the face moves are invalid!
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
        pcx := fc.Phase3CenterCoord(2, 5, Axis((Ord(a) div 2) * 2));
        pcy := fc.Phase3CenterCoord(3, 5, Axis((Ord(a) div 2) * 2));
        for k := 0 to 3 do
        begin
          fc.move(a, 0); // facemove
          if k <> 3 then
          begin
            pcxnew := fc.Phase3CenterCoord(2, 5, Axis((Ord(a) div 2) * 2));
            pcynew := fc.Phase3CenterCoord(3, 5, Axis((Ord(a) div 2) * 2));
            if (pcx = pcxnew) and (pcy = pcynew) then
            begin
              Ph3faceMoveAllowed[br, 3 * Ord(a) + k] := True;
              //Inc(ttt);
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




end.








