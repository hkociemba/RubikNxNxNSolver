unit phase4_tables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cubedefs;

procedure createNextMovePhase4Table;
procedure createPh4CenterMoveTable;
procedure createPhase4RLFBBrickMoveTable;
procedure createPhase4UDBrickMoveTable;
procedure createPh4UDPlusCrossPruningTable;
procedure createPh4UDCentBrickPruningTable;

var
   nextMovePhase4Arr: array [fU1 .. NoMove, InitMove .. yB3] of moves;
   Phase4CenterMove: array of array of UInt32;

   Phase4RLFBBrickMove: array of array of UInt16;
   Phase4UDBrickMove: array of array of UInt16;

   Ph4UDPlusCrossPrun: array of byte;
   Ph4UDCentBrickPrun: array of byte;

implementation
uses facecube, main, Forms;

procedure createNextMovePhase4Table;
var
  fc: faceletCube;
  mprev, mcurr: moves;
begin
  fc := faceletCube.Create(nil, 11); // any odd value possible
  for mcurr := InitMove to yB3 do
    // use NoMove for the predecessor if we have the first move
  begin
    if not Phase4Allowed[Ord(mcurr)] then
      continue;
    nextMovePhase4Arr[NoMove, mcurr] := fc.nextMovePh4(0, mcurr);
  end;
  for mprev := fU1 to yB3 do
  begin
    fc.fxymoves[0] := mprev;
    for mcurr := InitMove to yB3 do
    begin
      if not Phase4Allowed[Ord(mcurr)] then
        continue;
      nextMovePhase4Arr[mprev, mcurr] := fc.nextMovePh4(1, mcurr);
    end;
  end;
  fc.Free;
end;

procedure createPh4CenterMoveTable;
var
  i, j, k: integer;
  a: Axis;
  n: UInt32;
  fc: faceletcube;
begin
  fc := faceletCube.Create(nil, 11); // 11 arbitrary
  SetLength(Phase4CenterMove, B_8_4, 3 * 18);
  n := High(UInt32);

    for i := 0 to B_8_4 - 1 do
      for k := 0 to 53 do
        Phase4CenterMove[i, k] := n;

    for i := 0 to B_8_4 - 1 do
    begin
      fc.InvPhase3CenterCoord(i, 2, 3, U);//2, 3 arbitrary
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 0); // Faceturns
          if (k <> 3) and Phase4Allowed[3 * Ord(a) + k] then
            Phase4CenterMove[ i, 3 * Ord(a) + k] :=
              fc.Phase3CenterCoord(2, 3, U);
        end;
      end;
    end;


    for i := 0 to B_8_4 - 1 do
    begin
      fc.InvPhase3CenterCoord(i, 2, 3,U);
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 2); //x moves
          if (k <> 3) and Phase4Allowed[3 * Ord(a) + k + 18] then
            Phase4CenterMove[ i, 3 * Ord(a) + k + 18] :=
              fc.Phase3CenterCoord(2, 3, U);
        end;
      end;
    end;

    for i := 0 to B_8_4 - 1 do
    begin
      fc.InvPhase3CenterCoord(i, 2, 3,U);
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 3);//y moves
          if (k <> 3) and Phase4Allowed[3 * Ord(a) + k + 36] then
            Phase4CenterMove[ i, 3 * Ord(a) + k + 36] :=
              fc.Phase3CenterCoord(2, 3, U);
        end;
      end;
    end;

  fc.Free;
end;

procedure createPhase4RLFBBrickMoveTable;
var
  i, j, k: Integer;
  a: Axis;
  n: UInt16;
   fc: faceletcube;
begin
   fc := faceletCube.Create(nil, 11); // 11 arbitrary
  SetLength(Phase4RLFBBrickMove,256, 3 * 18);

  n := High(UInt16);
    for i := 0 to 255 do
      for k := 0 to 53 do
        Phase4RLFBBrickMove[ i, k] := n;


  //face moves do not change this brick coordinate


    for i := 0 to 255 do
    begin
      fc.InvPhase4RLFBBrickCoord(i, 2, 3);
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 2); //x-slice move
          if (k <> 3) and Phase4Allowed[3 * Ord(a) + k +18] then
            Phase4RLFBBrickMove[ i, 3 * Ord(a) + k+18] :=
              fc.Phase4RLFBBrickCoord(2, 3);
        end;
      end;
    end;

     for i := 0 to 255 do
    begin
      fc.InvPhase4RLFBBrickCoord(i, 2, 3);
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 3); //y-slice move
          if (k <> 3) and Phase4Allowed[3 * Ord(a) + k +36] then
            Phase4RLFBBrickMove[ i, 3 * Ord(a) + k+36] :=
              fc.Phase4RLFBBrickCoord(2, 3);
        end;
      end;
    end;

                              //faceletCube.Phase4UDBrickCoord(x, y: Integer)
  fc.Free;
end;

procedure createPhase4UDBrickMoveTable;   { TODO : file anlegen }
var
  i, j, k: Integer;
  a: Axis;
  n: UInt16;
  fc: faceletcube;
begin
   fc := faceletCube.Create(nil, 11); // 11 arbitrary
  SetLength(Phase4UDBrickMove,4900, 3 * 18);

  n := High(UInt16);
    for i := 0 to 4900-1 do
      for k := 0 to 53 do
        Phase4UDBrickMove[ i, k] := n;

    for i := 0 to 4900-1 do
    begin
      fc.InvPhase4UDBrickCoord(i, 2, 3);//2,3 arbitrary
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 0); //face move
          if (k <> 3) and Phase4Allowed[3 * Ord(a) + k ] then
            Phase4UDBrickMove[ i, 3 * Ord(a) + k] :=
              fc.Phase4UDBrickCoord(2, 3);
        end;
      end;
    end;

    for i := 0 to 4900-1 do
    begin
      fc.InvPhase4UDBrickCoord(i, 2, 3);//2,3 arbitrary
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 2); //x move
          if (k <> 3) and Phase4Allowed[3 * Ord(a) + k +18] then
            Phase4UDBrickMove[ i, 3 * Ord(a) + k+18] :=
              fc.Phase4UDBrickCoord(2, 3);
        end;
      end;
    end;

    for i := 0 to 4900-1 do
    begin
      fc.InvPhase4UDBrickCoord(i, 2, 3);//2,3 arbitrary
      for a := U to B do
      begin
        for k := 0 to 3 do
        begin
          fc.move(a, 3); //y move
          if (k <> 3) and Phase4Allowed[3 * Ord(a) + k +36] then
            Phase4UDBrickMove[ i, 3 * Ord(a) + k+36] :=
              fc.Phase4UDBrickCoord(2, 3);
        end;
      end;
    end;
  fc.Free;
end;





procedure createPh4UDPlusCrossPruningTable;
var
  i, depth, done: integer;
  b, b1, c,c1: UInt16;
  mv: Moves;
  a: Axis;
  // fs: TFileStream;
const
  NSTATES = 16 * 70;//
begin
  SetLength(Ph4UDPlusCrossPrun, NSTATES);

  for i := 0 to NSTATES - 1 do
    Ph4UDPlusCrossPrun[i] := $FF;

  Ph4UDPlusCrossPrun[0] := 0;
  done := 1;
  depth := 0;
  while done <> NSTATES do
  begin
    for b:=0 to 16-1 do
    begin
      for c :=0 to 70-1 do
      begin

        if Ph4UDPlusCrossPrun[16*c+b]= depth then
          begin
            for mv:=fU1 to xB3 do
            begin
               if not Phase4Allowed[Ord(mv)] then
            continue;
               case mv of
                fU1..fB3:
                begin
                  b1:=b;//allowed face moves do not change this
                  c1:= Phase4CenterMove[c,Ord(mv)]
                end;
                 xU1..xB3:
                begin
                  b1:= Phase4RLFBBrickMove[b,Ord(mv)+18];// must use y-bricks internally for 0<=b<16
                  c1:= Phase4CenterMove[c,Ord(mv)];
                end;
                yU1..yB3:
                begin
                  continue; //no y moves for orbit (x, size div 2)
                end;
              end;
              if  Ph4UDPlusCrossPrun[16*c1+b1]=$FF then
                begin
                   Ph4UDPlusCrossPrun[16*c1+b1]:= depth+1;
                   Inc(done);
                end;
            end;
          end

      end;//c
    end;//b
    Inc(depth);//1,5,35,159,506,950,1112,1120 , 7 moves maximum
  end;
end;

procedure createPh4UDCentBrickPruningTable;
var
  j, depth, cx, cy, bxy, cx1, cy1,bxy1: UInt32;
  i, done, idx, idx1: UInt64;
  a: Axis;
  mv:Moves;
  fs: TFileStream;
  fName: String;
const
  NSTATES = 24010000;//B_8_4 * B_8_4 * B_8_4 * B_8_4;
begin
  SetLength(Ph4UDCentBrickPrun, NSTATES); //wird bei formcreate gemacht
  fName := 'Ph4UDCentBrickPrun';
  if FileExists(fName) then
  begin
    fs := TFileStream.Create(fName, fmOpenRead);
    fs.ReadBuffer(Ph4UDCentBrickPrun[0], NSTATES);
    fs.Free;
  end
  else
  begin
    for i := 0 to NSTATES - 1 do
      Ph4UDCentBrickPrun[i] := $FF;
    // Sollte 0 sein
    Ph4UDCentBrickPrun[0] := 0; // distance to ID
    done := 1;
    depth := 0;
    while done <> NSTATES do
    begin
      for bxy := 0 to B_8_4 * B_8_4 - 1 do
        for cx := 0 to B_8_4 - 1 do
          for cy := 0 to B_8_4 - 1 do
          begin
            idx := B_8_4 * (B_8_4 * bxy + cx) + cy;
            if Ph4UDCentBrickPrun[idx] = depth then
            begin


            for mv := fU1 to yB3 do
            begin
               if not Phase4Allowed[Ord(mv)] then
                    continue;
               cx1:=Phase4CenterMove[cx,Ord(mv)];
               bxy1:= Phase4UDBrickMove[bxy,Ord(mv)];
              case mv of
                fU1..fB3:
                begin
                  cy1 :=Phase4CenterMove[cy,Ord(mv)];
                end;
                xU1..xB3:
                begin
                   cy1:=Phase4CenterMove[cy,Ord(mv)+18];
                end;
                yU1..yB3:
                begin
                   cy1:=Phase4CenterMove[cy,Ord(mv)-18];
                end;
              end;
               idx1 := B_8_4 * (B_8_4 * bxy1 + cx1) + cy1;

               if Ph4UDCentBrickPrun[idx1] = $FF then
                  begin
                    Ph4UDCentBrickPrun[idx1] := depth + 1;
                    Inc(done);
                  end;
             end;

            end;
          end;
        Inc(depth);//1,9,69,409,2245,12255,63377,302639,1285035,4476289,11651777,20140249,23658546,24000834,24009916,24010000
    end; //maximum 15 moves
    fs := TFileStream.Create(fName, fmCreate);
    fs.WriteBuffer(Ph4UDCentBrickPrun[0], NSTATES);
    fs.Free;
  end;
end;



end.

