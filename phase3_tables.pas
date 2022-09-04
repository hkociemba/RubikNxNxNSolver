unit phase3_tables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cubedefs;

procedure createNextMovePhase3Table;
procedure createPhase3CenterMoveTable;

var
  nextMovePhase3Arr: array [fU1 .. NoMove, InitMove .. yB3] of moves;
  Phase3CenterMove: array of array of array of UInt32;


implementation

uses facecube;

procedure createNextMovePhase3Table;
var
  fc: faceletCube;
  mprev, mcurr: moves;

  tt: integer;
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
      for k := 0 to 17 do
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




end.

