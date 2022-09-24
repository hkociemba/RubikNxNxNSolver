unit facecube;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, cubedefs;

function Cnk(n, k: integer): integer;

type
  faceletCube = class
  private
  public
    cv: TCanvas; // canvas auf der gezeichnet wird
    size: integer; // number of cubies on an edge
    pix: integer; // number of pixels for 1/3 facelet
    // face 0..5,row index 0..size-1, column index 0..size-1
    faceCols: array of array of array of ColorIndex;
    //tmoves: array of UInt16;
    cubiCorn: array [URF .. DRB] of OrientedCorner;
    cubiEdge: array [UR .. BR] of OrientedEdge;
    ecls: array of array of byte; //array for edgeclusters



    mvIdx: integer; // Helper variables for search function
    found: boolean;
    fxymoves: array [0 .. 25] of moves; // f-moves und xy-slice moves
    // permutations of the remapped edgecluster (0,y). filled with getEdgeCluster(y)

    //procedure setSize(s: Integer);
    function getSize: integer;
    procedure initCluster(x, y: integer);
    // slice move 0<=slice<size
    procedure move(a: Axis; slice: integer);
    // Wendet die Symmetrie auf das Cluster (x,y) an
    procedure applySymmetry(x, y: integer; s: Symmetry);
    procedure applySymmetryByIndex(x, y: integer; idx: integer);
    procedure applyInvSymmetryByIndex(x, y: integer; idx: integer);
    //create corners on the cubie level
    function setCornerCubies: CornerStatus;
    //create middle edges on the cubie level
    function setMiddleEdgeCubies: EdgeStatus;
    function cornerParityEven: boolean;
    function edgeParity(y: integer): integer;
    procedure DrawCube(xOff, yOff: integer);
    procedure drawPara1(x, y: integer);
    procedure drawPara2(x, y: integer);
    procedure drawSquare(x, y: integer);

    // phase 1
    function nextMovePh1(idx: integer; currMove: moves): moves;
    function Phase1CenterCoord(x, y: integer): integer; // 0<=cc<735471
    procedure InvPhase1CenterCoord(cc, x, y: integer);

    function Phase1Brick256Coord(x, y: integer): integer;
    procedure InvPhase1Brick256Coord(cc, x, y: integer);

    function MakeUDPlusCross1(x: integer): boolean; // Findet Zugfolge
    procedure SearchUDPlusCross1(cc, togo: integer);

    function MakeUDXCross(x: integer): boolean;
    procedure SearchUDXCross(ccx, slx, togo: integer);

    // phase 2
    function nextMovePh2(idx: integer; currMove: moves): moves;
    function Phase2CenterCoord(x, y: integer): integer; // 0<=cc<12870
    procedure InvPhase2CenterCoord(cc, x, y: integer);

    function Phase2SliceCoord(x, y: integer): integer; // 0<=cc<16
    procedure InvPhase2SliceCoord(cc, x, y: integer);

    function MakeFBPlusCross(x: integer): boolean; // Findet Zugfolge
    procedure SearchFBPlusCross(cc, ep, togo: integer);
    function MakeFBFullCenter(x, y: integer): boolean;
    procedure SearchFBFullCenter(ccx, slx, ccy, togo: integer);
    function MakeFBXCross(x: integer): boolean;
    procedure SearchFBXCross(ccx, sly, togo: integer);

    // phase 3
    function nextMovePh3(idx: integer; currMove: moves): moves;
    function Phase3CenterCoord(x, y: integer; a: Axis): integer;
    procedure InvPhase3CenterCoord(cc, x, y: integer; a: Axis);
    function Ph3RLFBCenterCoord(x, y: integer): UInt16;
    procedure InvPh3RLFBCenterCoord(cc, x, y: integer);

    function getPh3Brick702RLFBCentDepth(x, y: integer): integer;


    function Ph3Brick702Coord(x: integer): integer;
    procedure InvPh3Brick702Coord(br, x: integer);
    function MakePh3Cent702(x, y: integer): boolean;
    procedure SearchPh3Cent702(bx, by, cx, cy, dx, dy, togo: integer);
    function MakePh3XCross(x: integer): boolean;
    procedure SearchPh3XCross(bx, cx, togo: integer);
    function MakePh3RLFBPlusCross(x: integer): boolean;
    procedure SearchPh3RLFBPlusCross(bx, togo: integer);

    // phase 4
    function nextMovePh4(idx: integer; currMove: moves): moves;
    function MakePh4UDPlusCross(x: integer): boolean;
    procedure SearchPh4UDPlusCross(c, b, togo: integer);
    function MakePh4UDCenters(x, y: integer): boolean;
    procedure SearchPh4UDCenter(cx, cy, bxy, bo, togo: integer);
    function MakePh4XCross(x: integer): boolean;
    procedure SearchPh4XCross(cx, b, bo, togo: integer);

    function Phase4RLFBBrickCoord(x, y: integer): integer;
    procedure InvPhase4RLFBBrickCoord(cc, x, y: integer);

    function Phase4UDBrickCoord(x, y: integer): integer;
    procedure InvPhase4UDBrickCoord(cc, x, y: integer);

    //phase 5
    procedure edgemove(x: integer; mv: Moves);
    procedure applyMovesEdges;
    function MakeFledge:Boolean;
    procedure SearchFLEdge(x, sliceState, dist: byte; togo: integer);
    function badEdgeCnt(e: Edge): integer;
    procedure store(ed: Edge);


    //unused

    //function getUDBrickXYDepth(x, y: integer): integer;
    //function getUDBrickXCentXDepth(x, y: integer): integer;
    //function SearchUDCent(x, y: integer): boolean;
    //procedure SearchUDCenter(bx, cx, by, cy, bxby_dist, bxcx_dist,
    //  bycy_dist, togo: integer);
    //function Phase1BrickCoord(x :integer): integer; // 0<=cc<735471
    //procedure InvPhase1BrickCoord(cc, x: integer);




    procedure printMoves(i, j: integer);
    procedure applyMoves(i, j: integer);

    function clusterColorIndex(x, y, i: integer): ColorIndex;
    procedure setClusterColorIndex(x, y, i: integer; col: ColorIndex);
    procedure getEdgeCluster(y: integer);

    constructor Create(cvas: TCanvas; sz: integer); overload;
    // creates cube with odd size
    constructor Create(fc: faceletCube); overload;

  end;

//var

//phase 1
//1<=x<y<(size-1)/2
//UDXCrossMoveX: array of array of UInt32;
//UDXCrossMoveF: array of array of UInt32;


implementation

uses Windows, main, Forms, phase1_tables, phase2_tables, phase3_tables,
  phase4_tables, phase5_tables;
// Wendet die Symmetrie mit dem Index 0..15 auf Cluster(x,y) an
// idx = 8*LR2 + 4*F2 + U4
procedure faceletCube.applySymmetryByIndex(x, y, idx: integer);
var
  s, i: integer;
begin
  s := idx div 8;
  for i := 0 to s - 1 do
    applySymmetry(x, y, S_LR2);
  idx := idx mod 8;

  s := idx div 4;
  for i := 0 to s - 1 do
    applySymmetry(x, y, S_F2);
  s := idx mod 4;

  for i := 0 to s - 1 do
    applySymmetry(x, y, S_U4);
end;

// Wendet die die inverse Symmetrie zu dem Index 0..7 auf Cluster(x,y) an
procedure faceletCube.applyInvSymmetryByIndex(x, y, idx: integer);
var
  s, i: integer;
begin
  s := 3 - idx mod 4;
  for i := 0 to s do
    applySymmetry(x, y, S_U4);

  idx := idx div 4;
  s := 1 - idx mod 2;
  for i := 0 to s do
    applySymmetry(x, y, S_F2);

  s := 1 - idx div 2;
  for i := 0 to s do
    applySymmetry(x, y, S_LR2);
end;



// next move for xy-cluster
// Dazu muss die  UDPlusCrossCoord ID sein für die Zugfolge der Länge idx-1
function faceletCube.nextMovePh1(idx: integer; currMove: moves): moves;
var
  pm: moves;
begin
  if currMove = yB3 then // done
    Exit(NoMove);
  if idx = 0 then
    Exit(Succ(currMove))
  else
  begin
    pm := fxymoves[idx - 1]; // predecessor
    while True do
    begin
      currMove := Succ(currMove);

      if currMove = NoMove then
        Exit(NoMove);

      if Ord(pm) < Ord(xU1) then //previous move is face move
      begin
        if (Ord(currMove) <= Ord(pm)) then
          //all face moves commute restricted to the centers
          continue;
        if Ord(currMove) >= Ord(xU1) then  //face move followed by slice move
          // always valid
          Exit(currMove)
        else // pm<currMove<xU1
        begin
          if Ord(pm) div 3 = Ord(currMove) div 3 then
            // same face
            continue
          else
            Exit(currMove);
        end;
      end;

      //Ord(pm) >= Ord(xU1), previous move is slice move
      if (Ord(pm) div 6) mod 3 <> (Ord(currMove) div 6) mod 3 then
        Exit(currMove);
      // pm and currMove are on different axes and hence do not commute


      // both moves are on the same axis and commute
      // we can force an order
      if Ord(currMove) <= Ord(pm) then
        continue;

      // if the  prefixes f,x,y are different for both moves, currmove is valid
      if Ord(currMove) div 18 <> Ord(pm) div 18 then
        Exit(currMove);

      // we have the same prefix and the same axis
      if (Ord(currMove) mod 6) div 3 <> (Ord(pm) mod 6) div 3 then
        // moves are on different slices of the axis
        Exit(currMove)
      else
        continue;
    end;
  end;
end;

procedure faceletCube.applySymmetry(x, y: integer; s: Symmetry);
var
  tmp: ColorIndex;
  a: Axis;
begin
  case s of

    S_URF3:
    begin
      tmp := faceCols[Ord(U), x, y];
      faceCols[Ord(U), x, y] := faceCols[Ord(F), size - 1 - y, x];
      faceCols[Ord(F), size - 1 - y, x] := faceCols[Ord(R), size - 1 - x, size - 1 - y];
      faceCols[Ord(R), size - 1 - x, size - 1 - y] := tmp;

      tmp := faceCols[Ord(D), x, y];
      faceCols[Ord(D), x, y] := faceCols[Ord(B), size - 1 - y, x];
      faceCols[Ord(B), size - 1 - y, x] := faceCols[Ord(L), x, y];
      faceCols[Ord(L), x, y] := tmp;

      if (y = x) and (size - 1 = 2 * x) then
        exit;

      tmp := faceCols[Ord(U), y, size - 1 - x];
      faceCols[Ord(U), y, size - 1 - x] := faceCols[Ord(F), x, y];
      faceCols[Ord(F), x, y] := faceCols[Ord(R), size - 1 - y, x];
      faceCols[Ord(R), size - 1 - y, x] := tmp;

      tmp := faceCols[Ord(U), size - 1 - x, size - 1 - y];
      faceCols[Ord(U), size - 1 - x, size - 1 - y] := faceCols[Ord(F), y, size - 1 - x];
      faceCols[Ord(F), y, size - 1 - x] := faceCols[Ord(R), x, y];
      faceCols[Ord(R), x, y] := tmp;

      tmp := faceCols[Ord(U), size - 1 - y, x];
      faceCols[Ord(U), size - 1 - y, x] := faceCols[Ord(F), size - 1 - x, size - 1 - y];
      faceCols[Ord(F), size - 1 - x, size - 1 - y] := faceCols[Ord(R), y, size - 1 - x];
      faceCols[Ord(R), y, size - 1 - x] := tmp;

      tmp := faceCols[Ord(D), y, size - 1 - x];
      faceCols[Ord(D), y, size - 1 - x] := faceCols[Ord(B), x, y];
      faceCols[Ord(B), x, y] := faceCols[Ord(L), y, size - 1 - x];
      faceCols[Ord(L), y, size - 1 - x] := tmp;

      tmp := faceCols[Ord(D), size - 1 - x, size - 1 - y];
      faceCols[Ord(D), size - 1 - x, size - 1 - y] := faceCols[Ord(B), y, size - 1 - x];
      faceCols[Ord(B), y, size - 1 - x] := faceCols[Ord(L), size - 1 - x, size - 1 - y];
      faceCols[Ord(L), size - 1 - x, size - 1 - y] := tmp;

      tmp := faceCols[Ord(D), size - 1 - y, x];
      faceCols[Ord(D), size - 1 - y, x] := faceCols[Ord(B), size - 1 - x, size - 1 - y];
      faceCols[Ord(B), size - 1 - x, size - 1 - y] := faceCols[Ord(L), size - 1 - y, x];
      faceCols[Ord(L), size - 1 - y, x] := tmp;
    end;

    S_F2:
    begin

      tmp := faceCols[Ord(U), x, y];
      faceCols[Ord(U), x, y] := faceCols[Ord(D), size - 1 - x, size - 1 - y];
      faceCols[Ord(D), size - 1 - x, size - 1 - y] := tmp;

      tmp := faceCols[Ord(R), x, y];
      faceCols[Ord(R), x, y] := faceCols[Ord(L), size - 1 - x, size - 1 - y];
      faceCols[Ord(L), size - 1 - x, size - 1 - y] := tmp;

      if (y = x) and (size - 1 = 2 * x) then
        exit;//center facelets for odd cubes

      tmp := faceCols[Ord(U), y, size - 1 - x];
      faceCols[Ord(U), y, size - 1 - x] := faceCols[Ord(D), size - 1 - y, x];
      faceCols[Ord(D), size - 1 - y, x] := tmp;

      tmp := faceCols[Ord(U), size - 1 - x, size - 1 - y];
      faceCols[Ord(U), size - 1 - x, size - 1 - y] := faceCols[Ord(D), x, y];
      faceCols[Ord(D), x, y] := tmp;

      tmp := faceCols[Ord(U), size - 1 - y, x];
      faceCols[Ord(U), size - 1 - y, x] := faceCols[Ord(D), y, size - 1 - x];
      faceCols[Ord(D), y, size - 1 - x] := tmp;



      tmp := faceCols[Ord(F), x, y];
      faceCols[Ord(F), x, y] := faceCols[Ord(F), size - 1 - x, size - 1 - y];
      faceCols[Ord(F), size - 1 - x, size - 1 - y] := tmp;

      tmp := faceCols[Ord(F), y, size - 1 - x];
      faceCols[Ord(F), y, size - 1 - x] := faceCols[Ord(F), size - 1 - y, x];
      faceCols[Ord(F), size - 1 - y, x] := tmp;

      tmp := faceCols[Ord(B), x, y];
      faceCols[Ord(B), x, y] := faceCols[Ord(B), size - 1 - x, size - 1 - y];
      faceCols[Ord(B), size - 1 - x, size - 1 - y] := tmp;

      tmp := faceCols[Ord(B), y, size - 1 - x];
      faceCols[Ord(B), y, size - 1 - x] := faceCols[Ord(B), size - 1 - y, x];
      faceCols[Ord(B), size - 1 - y, x] := tmp;




      tmp := faceCols[Ord(R), y, size - 1 - x];
      faceCols[Ord(R), y, size - 1 - x] := faceCols[Ord(L), size - 1 - y, x];
      faceCols[Ord(L), size - 1 - y, x] := tmp;

      tmp := faceCols[Ord(R), size - 1 - x, size - 1 - y];
      faceCols[Ord(R), size - 1 - x, size - 1 - y] := faceCols[Ord(L), x, y];
      faceCols[Ord(L), x, y] := tmp;

      tmp := faceCols[Ord(R), size - 1 - y, x];
      faceCols[Ord(R), size - 1 - y, x] := faceCols[Ord(L), y, size - 1 - x];
      faceCols[Ord(L), y, size - 1 - x] := tmp;
    end;
    S_U4:
    begin
      tmp := faceCols[Ord(U), x, y];
      faceCols[Ord(U), x, y] := faceCols[Ord(U), size - 1 - y, x];
      faceCols[Ord(U), size - 1 - y, x] :=
        faceCols[Ord(U), size - 1 - x, size - 1 - y];
      faceCols[Ord(U), size - 1 - x, size - 1 - y] :=
        faceCols[Ord(U), y, size - 1 - x];
      faceCols[Ord(U), y, size - 1 - x] := tmp;

      tmp := faceCols[Ord(D), x, y];
      faceCols[Ord(D), x, y] := faceCols[Ord(D), y, size - 1 - x];
      faceCols[Ord(D), y, size - 1 - x] :=
        faceCols[Ord(D), size - 1 - x, size - 1 - y];
      faceCols[Ord(D), size - 1 - x, size - 1 - y] :=
        faceCols[Ord(D), size - 1 - y, x];
      faceCols[Ord(D), size - 1 - y, x] := tmp;

      tmp := faceCols[Ord(F), x, y];
      faceCols[Ord(F), x, y] := faceCols[Ord(R), x, y];
      faceCols[Ord(R), x, y] := faceCols[Ord(B), x, y];
      faceCols[Ord(B), x, y] := faceCols[Ord(L), x, y];
      faceCols[Ord(L), x, y] := tmp;
      if (y = x) and (size - 1 = 2 * x) then
        exit;//center facelets for odd cubes

      tmp := faceCols[Ord(F), y, size - 1 - x];
      faceCols[Ord(F), y, size - 1 - x] := faceCols[Ord(R), y, size - 1 - x];
      faceCols[Ord(R), y, size - 1 - x] := faceCols[Ord(B), y, size - 1 - x];
      faceCols[Ord(B), y, size - 1 - x] := faceCols[Ord(L), y, size - 1 - x];
      faceCols[Ord(L), y, size - 1 - x] := tmp;

      tmp := faceCols[Ord(F), size - 1 - x, size - 1 - y];
      faceCols[Ord(F), size - 1 - x, size - 1 - y] :=
        faceCols[Ord(R), size - 1 - x, size - 1 - y];
      faceCols[Ord(R), size - 1 - x, size - 1 - y] :=
        faceCols[Ord(B), size - 1 - x, size - 1 - y];
      faceCols[Ord(B), size - 1 - x, size - 1 - y] :=
        faceCols[Ord(L), size - 1 - x, size - 1 - y];
      faceCols[Ord(L), size - 1 - x, size - 1 - y] := tmp;

      tmp := faceCols[Ord(F), size - 1 - y, x];
      faceCols[Ord(F), size - 1 - y, x] := faceCols[Ord(R), size - 1 - y, x];
      faceCols[Ord(R), size - 1 - y, x] := faceCols[Ord(B), size - 1 - y, x];
      faceCols[Ord(B), size - 1 - y, x] := faceCols[Ord(L), size - 1 - y, x];
      faceCols[Ord(L), size - 1 - y, x] := tmp;
    end;
    S_LR2:  //Only valid for clusters with reflectional symmetry, x or +cluster!
    begin
      for a := U to B do
      begin
        if (a = R) or (a = L) then
          continue;
        if y <> size - 1 - y then
        begin
          tmp := faceCols[Ord(a), x, y];
          faceCols[Ord(a), x, y] := faceCols[Ord(a), x, size - 1 - y];
          faceCols[Ord(a), x, size - 1 - y] := tmp;

          tmp := faceCols[Ord(a), size - 1 - x, size - 1 - y];
          faceCols[Ord(a), size - 1 - x, size - 1 - y] :=
            faceCols[Ord(a), size - 1 - x, y];
          faceCols[Ord(a), size - 1 - x, y] := tmp;

          tmp := faceCols[Ord(a), size - 1 - y, x];
          faceCols[Ord(a), size - 1 - y, x] :=
            faceCols[Ord(a), size - 1 - y, size - 1 - x];
          faceCols[Ord(a), size - 1 - y, size - 1 - x] := tmp;
        end;

        tmp := faceCols[Ord(a), y, size - 1 - x];
        faceCols[Ord(a), y, size - 1 - x] := faceCols[Ord(a), y, x];
        faceCols[Ord(a), y, x] := tmp;
      end;

      tmp := faceCols[Ord(L), x, y];
      faceCols[Ord(L), x, y] := faceCols[Ord(R), x, size - 1 - y];
      faceCols[Ord(R), x, size - 1 - y] := tmp;

      if (y = x) and (size - 1 = 2 * x) then
        exit;//center facelets for odd cubes

      tmp := faceCols[Ord(L), y, size - 1 - x];
      faceCols[Ord(L), y, size - 1 - x] := faceCols[Ord(R), y, x];
      faceCols[Ord(R), y, x] := tmp;

      tmp := faceCols[Ord(L), size - 1 - x, size - 1 - y];
      faceCols[Ord(L), size - 1 - x, size - 1 - y] :=
        faceCols[Ord(R), size - 1 - x, y];
      faceCols[Ord(R), size - 1 - x, y] := tmp;

      tmp := faceCols[Ord(L), size - 1 - y, x];
      faceCols[Ord(L), size - 1 - y, x] :=
        faceCols[Ord(R), size - 1 - y, size - 1 - x];
      faceCols[Ord(R), size - 1 - y, size - 1 - x] := tmp;

      if y <> size - 1 - y then
      begin
        tmp := faceCols[Ord(R), x, y];
        faceCols[Ord(R), x, y] := faceCols[Ord(L), x, size - 1 - y];
        faceCols[Ord(L), x, size - 1 - y] := tmp;

        tmp := faceCols[Ord(R), y, size - 1 - x];
        faceCols[Ord(R), y, size - 1 - x] := faceCols[Ord(L), y, x];
        faceCols[Ord(L), y, x] := tmp;

        tmp := faceCols[Ord(R), size - 1 - x, size - 1 - y];
        faceCols[Ord(R), size - 1 - x, size - 1 - y] :=
          faceCols[Ord(L), size - 1 - x, y];
        faceCols[Ord(L), size - 1 - x, y] := tmp;

        tmp := faceCols[Ord(R), size - 1 - y, x];
        faceCols[Ord(R), size - 1 - y, x] :=
          faceCols[Ord(L), size - 1 - y, size - 1 - x];
        faceCols[Ord(L), size - 1 - y, size - 1 - x] := tmp;
      end;
    end;

    S_R4:
      ;
    S_F4:
      ;
  end;
  Form1.PaintBoxFaces.Invalidate;
end;



function Cnk(n, k: integer): integer;
var
  s, j: integer;
begin
  if n < k then
    Result := 0
  else
  begin
    s := 1;
    if (k > n div 2) then
      k := n - k;
    for j := 1 to k do
    begin
      s := (s * n) div j;
      n := n - 1;
    end;
    Result := s;
  end;
end;


// Gives  the colorIndex of facelet 0<=i<24 for the cluster 0<=x,y<N
// x counts from upper left edge down, y to the right
function faceletCube.clusterColorIndex(x, y, i: integer): ColorIndex;
var
  faceIdx, face, xf, yf: integer;
begin
  xf := 0;
  yf := 0;//initialize to prevent compiler warnings
  faceIdx := i mod 4;
  case faceIdx of
    0:
    begin
      xf := x;
      yf := y;
    end;
    1:
    begin
      xf := y;
      yf := size - 1 - x;
    end;
    2:
    begin
      xf := size - 1 - x;
      yf := size - 1 - y;
    end;
    3:
    begin
      xf := size - 1 - y;
      yf := x;
    end;
  end;
  face := i div 4;
  Result := faceCols[face, xf, yf];
end;


procedure faceletCube.setClusterColorIndex(x, y, i: integer; col: ColorIndex);
//i is 0..23 and describes all facelets within the cluster which has one facelet
//at coordinate (x,y)
var
  faceIdx, face, xf, yf: integer;
begin
  xf := 0;
  yf := 0;//initialize to prevent compiler warnings
  faceIdx := i mod 4;
  case faceIdx of
    0:
    begin
      xf := x;
      yf := y;
    end;
    1:
    begin
      xf := y;
      yf := size - 1 - x;
    end;
    2:
    begin
      xf := size - 1 - x;
      yf := size - 1 - y;
    end;
    3:
    begin
      xf := size - 1 - y;
      yf := x;
    end;
  end;
  face := i div 4;
  faceCols[face, xf, yf] := col;
end;


procedure faceletCube.move(a: Axis; slice: integer);
// 0<=slice<=size-1
var
  i, j: integer;
  tmp: array of array of ColorIndex;
  c: ColorIndex;
begin
  SetLength(tmp, size, size);

  // a <= F then
  case a of
    U, R, F:
    begin
      if slice = 0 then //face turn
      begin
        for i := 0 to size - 1 do
          for j := 0 to size - 1 do
          begin
            tmp[j, size - 1 - i] := faceCols[Ord(a), i, j];
          end;
        for i := 0 to size - 1 do
          for j := 0 to size - 1 do
          begin
            faceCols[Ord(a), i, j] := tmp[i, j];
          end;
      end
      else if slice = size - 1 then  //equivalent to inverse opposite face turn
      begin
        for i := 0 to size - 1 do
          for j := 0 to size - 1 do
          begin
            tmp[i, j] := faceCols[(Ord(a) + 1), j, size - 1 - i];
          end;
        for i := 0 to size - 1 do
          for j := 0 to size - 1 do
          begin
            faceCols[(Ord(a) + 1), i, j] := tmp[i, j];
          end;
      end;
    end;
  end;

  case a of
    U:
    begin
      for j := 0 to size - 1 do
      begin
        c := faceCols[Ord(F), slice, j];
        faceCols[Ord(F), slice, j] := faceCols[Ord(R), slice, j];
        faceCols[Ord(R), slice, j] := faceCols[Ord(B), slice, j];
        faceCols[Ord(B), slice, j] := faceCols[Ord(L), slice, j];
        faceCols[Ord(L), slice, j] := c;
      end;
    end;

    R:
    begin
      for j := 0 to size - 1 do
      begin
        c := faceCols[Ord(U), j, size - 1 - slice];
        faceCols[Ord(U), j, size - 1 - slice] :=
          faceCols[Ord(F), j, size - 1 - slice];
        faceCols[Ord(F), j, size - 1 - slice] :=
          faceCols[Ord(D), j, size - 1 - slice];
        faceCols[Ord(D), j, size - 1 - slice] :=
          faceCols[Ord(B), size - 1 - j, slice];
        faceCols[Ord(B), size - 1 - j, slice] := c;
      end;
    end;

    F:
    begin
      for j := 0 to size - 1 do
      begin
        c := faceCols[Ord(U), size - 1 - slice, j];
        faceCols[Ord(U), size - 1 - slice, j] :=
          faceCols[Ord(L), size - 1 - j, size - 1 - slice];
        faceCols[Ord(L), size - 1 - j, size - 1 - slice] :=
          faceCols[Ord(D), slice, size - 1 - j];
        faceCols[Ord(D), slice, size - 1 - j] := faceCols[Ord(R), j, slice];
        faceCols[Ord(R), j, slice] := c;
      end;
    end;
    D:
      for i := 1 to 3 do
        move(U, size - 1 - slice);
    L:
      for i := 1 to 3 do
        move(R, size - 1 - slice);
    B:
      for i := 1 to 3 do
        move(F, size - 1 - slice);

  end;
  Form1.PaintBoxFaces.Invalidate;
end;


function faceletCube.getSize: integer;
begin
  Result := size;
end;

procedure faceletCube.drawSquare(x, y: integer);
var
  p: array [1 .. 4] of TPoint;
begin
  p[1].x := x;
  p[1].y := y;
  p[2].x := x + 3 * pix;
  p[2].y := y;
  p[3].x := p[2].x;
  p[3].y := p[2].y + 3 * pix;
  p[4].x := p[3].x - 3 * pix;
  p[4].y := p[3].y;
  SetBkMode(cv.Handle, OPAQUE);
  // to set the hatched background
  SetBkColor(cv.Handle, clBlack);
  cv.Polygon(p);
  SetBkMode(cv.Handle, TRANSPARENT);
end;

procedure faceletCube.drawPara1(x, y: integer);
var
  p: array [1 .. 4] of TPoint;
begin
  p[1].x := x;
  p[1].y := y;
  p[2].x := x + 3 * pix;
  p[2].y := y;
  p[3].x := p[2].x - 2 * pix;
  p[3].y := p[2].y + 2 * pix;
  p[4].x := p[3].x - 3 * pix;
  p[4].y := p[3].y;
  SetBkMode(cv.Handle, OPAQUE);
  // to set the hatched background
  SetBkColor(cv.Handle, clBlack);
  cv.Polygon(p);
  SetBkMode(cv.Handle, TRANSPARENT);
end;

procedure faceletCube.drawPara2(x, y: integer);
var
  p: array [1 .. 4] of TPoint;
begin
  p[1].x := x;
  p[1].y := y;
  p[2].x := x + 2 * pix;
  p[2].y := y - 2 * pix;
  p[3].x := p[2].x;
  p[3].y := p[2].y + 3 * pix;
  p[4].x := p[1].x;
  p[4].y := p[1].y + 3 * pix;
  SetBkMode(cv.Handle, OPAQUE);
  // to set the hatched background
  SetBkColor(cv.Handle, clBlack);
  cv.Polygon(p);
  SetBkMode(cv.Handle, TRANSPARENT);
end;

procedure faceletCube.DrawCube(xOff, yOff: integer);
var
  i, j: integer;
begin
  // left face
  for i := 0 to size - 1 do
    for j := 0 to size - 1 do
    begin
      cv.Brush.Color := Color[faceCols[Ord(L), i, j]];
      drawSquare(xOff + 3 * j * pix, yOff + 2 * pix * size + 3 * i * pix);
    end;
  // front face
  for i := 0 to size - 1 do
    for j := 0 to size - 1 do
    begin
      cv.Brush.Color := Color[faceCols[Ord(F), i, j]];
      drawSquare(xOff + 3 * pix * size + 3 * j * pix, yOff + 2 * pix *
        size + 3 * i * pix);
    end;
  // down face
  for i := 0 to size - 1 do
    for j := 0 to size - 1 do
    begin
      cv.Brush.Color := Color[faceCols[Ord(D), i, j]];
      drawSquare(xOff + 3 * pix * size + 3 * j * pix, yOff + 5 * pix *
        size + 3 * i * pix);
    end;
  // back face
  for i := 0 to size - 1 do
    for j := 0 to size - 1 do
    begin
      cv.Brush.Color := Color[faceCols[Ord(B), i, j]];
      drawSquare(xOff + 8 * pix * size + 3 * j * pix, yOff + 3 * i * pix);
    end;

  // right face
  for i := 0 to size - 1 do
    for j := 0 to size - 1 do
    begin
      cv.Brush.Color := Color[faceCols[Ord(R), i, j]];
      drawPara2(xOff + 6 * pix * size + 2 * j * pix, yOff + 2 * pix *
        size + 3 * i * pix - 2 * j * pix);
    end;
  // up face
  for i := 0 to size - 1 do
    for j := 0 to size - 1 do
    begin
      cv.Brush.Color := Color[faceCols[Ord(U), i, j]];
      drawPara1(xOff + 5 * pix * size - 2 * pix * i + 3 * pix * j,
        yOff + 2 * i * pix);
    end;
end;

function faceletCube.cornerParityEven: boolean;
var
  i, j: Corner;
  s: integer;
begin
  s := 0;
  for i := DRB downto Succ(URF) do
    for j := Pred(i) downto URF do
      if cubiCorn[j].c > cubiCorn[i].c then
        Inc(s);
  if Odd(s) then
    Result := False
  else
    Result := True;
end;

// return the parity of the (0,y)
function faceletCube.edgeParity(y: integer): integer;
var
  i, j, s: integer;
begin
  getEdgeCluster(y);
  s := 0;
  for i := 0 to 22 do
    for j := i + 1 to 23 do
      if ecls[y, j] < ecls[y, i] then
        Inc(s);
  Result := s mod 2;
end;




function faceletCube.setCornerCubies: CornerStatus;
var
  i, j: Corner;
  ori: integer;
  col1, col2: ColorIndex;
begin
  for i := URF to DRB do
    cubiCorn[i].c := NNN;
  for i := URF to DRB do
  begin
    // get orientation of corner i
    ori := 0;
    while (faceCols[Ord(CCI[i, ori]), CFRow[i, ori] * (size - 1),
        CFCol[i, ori] * (size - 1)] <> UCol) and
      (faceCols[Ord(CCI[i, ori]), CFRow[i, ori] * (size - 1),
        CFCol[i, ori] * (size - 1)] <> DCol) and (ori < 2) do
      Inc(ori);
    // ori now contains orientation of corner i, provided ther is a DCol or
    //Ucol at corner i
    ori := (ori + 1) mod 3;
    col1 := faceCols[Ord(CCI[i, ori]), CFRow[i, ori] * (size - 1),
      CFCol[i, ori] * (size - 1)];
    ori := (ori + 1) mod 3;
    col2 := faceCols[Ord(CCI[i, ori]), CFRow[i, ori] * (size - 1),
      CFCol[i, ori] * (size - 1)];
    ori := (ori + 1) mod 3; // restore original orientation
    for j := URF to DRB do
      if (col1 = CCI[j, 1]) and (col2 = CCI[j, 2]) then
      begin
        cubiCorn[i].c := j; // corner j sits in corner i's clean cube position
        cubiCorn[i].o := ori; // twist of corner j
      end;
  end;
  //check if we have a valid corner configuration
  for i := URF to DRB do
    if cubiCorn[i].c = NNN then
      Exit(CORNPERMUTATIONERROR);
  ori := 0;
  for i := URF to DRB do
    Inc(ori, cubiCorn[i].o);
  if ori mod 3 <> 0 then
    Exit(CORNORIENTATIONERROR);
  Result := CORNNOERROR;
end;

function faceletCube.setMiddleEdgeCubies: EdgeStatus;
var
  i, j: Edge;
  ori, sz2: integer;
  col0, col1: ColorIndex;
begin
  if not Odd(size) then
    Exit(EDGESIZEERROR);
  sz2 := (size - 1) div 2;
  for i := UR to BR do
    cubiEdge[i].e := NN;
  for i := UR to BR do
  begin
    // get orientation of middle edge
    col0 := faceCols[Ord(ECI[i, 0]), EFRow[i, 0] * sz2, EFCol[i, 0] * sz2];
    col1 := faceCols[Ord(ECI[i, 1]), EFRow[i, 1] * sz2, EFCol[i, 1] * sz2];
    for j := UR to BR do
      if (col0 = ECI[j, 0]) and (col1 = ECI[j, 1]) then
      begin
        cubiEdge[i].e := j;
        cubiEdge[i].o := 0;
      end
      else if (col0 = ECI[j, 1]) and (col1 = ECI[j, 0]) then
      begin
        cubiEdge[i].e := j;
        cubiEdge[i].o := 1;
      end;
  end;
  for i := UR to BR do
    if cubiEdge[i].e = NN then
      Exit(EDGEPERMUTATIONERROR);
  ori := 0;
  for i := UR to BR do
    Inc(ori, cubiEdge[i].o);
  if Odd(ori) then
    Exit(EDGEORIENTATIONERROR);
  Result := EDGENOERROR;
end;

//set the solved position for a cluster
procedure faceletcube.initCluster(x, y: integer);
var
  i: integer;
begin
  for i := 0 to 23 do
    setClusterColorIndex(x, y, i, ColorIndex(i div 4));
end;

//return coodinate for index i (see cubedefs)
function getPos(i, cl, sz: integer): integer;
begin
  case i of
    0:
      Result := 0;
    1:
      Result := cl;
    2:
      Result := sz - 1;
    3:
      Result := sz - 1 - cl;
  end;
end;

procedure faceletCube.getEdgeCluster(y: integer);
var
  i, j, fc: integer;
  c: array [0 .. 1] of ColorIndex;
begin
  for i := 0 to 23 do
  begin
    fc := i div 4;
    c[0] := faceCols[Ord(ECCI[i, 0]), getPos(ECFRow[i, 0], y, size),
      getPos(ECFCol[i, 0], y, size)];
    c[1] := faceCols[Ord(ECCI[i, 1]), getPos(ECFRow[i, 1], y, size),
      getPos(ECFCol[i, 1], y, size)];
    j := 0;
    while (ECCI[j, 0] <> c[0]) or (ECCI[j, 1] <> c[1]) do
      Inc(j);
    ecls[y, remapEdges[i]] := remapEdges[j];
  end;

end;


constructor faceletCube.Create(cvas: TCanvas; sz: integer);
var
  i, j: integer;
  a: Axis;
  c: ColorIndex;
begin
  cv := cvas;
  size := sz;
  SetLength(faceCols, 6, size, size);
  for a := U to B do
  begin
    c := ColorIndex(Ord(a));
    for i := 0 to size - 1 do
      for j := 0 to size - 1 do
        faceCols[Ord(a), i, j] := c;
  end;

end;

// copy constructor, erzeugt aber immer einen cube mit ungerader size
constructor faceletCube.Create(fc: faceletCube);
var
  i, j: integer;
  a: Axis;
  c: ColorIndex;
begin
  cv := fc.cv; // Kann Probleme bereiten!
  if Odd(fc.size) then
  begin
    size := fc.size;
    SetLength(faceCols, 6, size, size);
    for a := U to B do
    begin
      c := ColorIndex(Ord(a));
      for i := 0 to size - 1 do
        for j := 0 to size - 1 do
          faceCols[Ord(a), i, j] := fc.faceCols[Ord(a), i, j];
    end;
  end
  else
  begin
    size := fc.size + 1;//for even sizes, add a middle slice
    SetLength(faceCols, 6, size, size);
    for a := U to B do
    begin
      c := ColorIndex(Ord(a));
      for i := 0 to (size - 1) div 2 - 1 do
        for j := 0 to (size - 1) div 2 - 1 do
          faceCols[Ord(a), i, j] := fc.faceCols[Ord(a), i, j];

      for i := 0 to (size - 1) div 2 - 1 do
        for j := (size - 1) div 2 to size - 2 do
          faceCols[Ord(a), i, j + 1] := fc.faceCols[Ord(a), i, j];

      for i := (size - 1) div 2 to size - 2 do
        for j := 0 to (size - 1) div 2 - 1 do
          faceCols[Ord(a), i + 1, j] := fc.faceCols[Ord(a), i, j];

      for i := (size - 1) div 2 to size - 2 do
        for j := (size - 1) div 2 to size - 2 do
          faceCols[Ord(a), i + 1, j + 1] := fc.faceCols[Ord(a), i, j];

      for i := 0 to size - 1 do
        faceCols[Ord(a), i, (size - 1) div 2] := c;
      for j := 0 to size - 1 do
        faceCols[Ord(a), (size - 1) div 2, j] := c;

      setCornerCubies;
      if not cornerParityEven then // zwei Mitteledges vertauschen
      begin
        faceCols[Ord(F), 0, (size - 1) div 2] := BCol;
        faceCols[Ord(B), 0, (size - 1) div 2] := FCol;
      end;

    end;
  end;
end;


//phase 1 implementation


//the coordinate of the "x-bricks", when moves which change only cluster (x,y) are
//applied
//function faceletCube.Phase1BrickCoord(x: integer): integer;
//begin
//  Result := Phase1CenterCoord(x, size div 2);
//end;


//procedure faceletCube.InvPhase1BrickCoord(cc,x: integer);
//begin
//  InvPhase1CenterCoord(cc, x, size div 2);
//end;



//A subgroup of the full brick group generated by only applying xR1,xF1,xL1,xB1
//and yR1,yF1,yL1,yB1 moves
function faceletCube.Phase1Brick256Coord(x, y: integer): integer;
var
  i, m, idx: integer;
  c: ColorIndex;
begin
  m := (size - 1) div 2; // Index der mittleren Slice
  idx := 0;
  for i := 0 to 3 do // Es reicht die Positionen von U zu betrachten
  begin
    c := clusterColorIndex(x, m, i);
    idx := 2 * idx;
    if (c <> UCol) and (c <> DCol) then
      Inc(idx); // slice gedreht
  end;
  for i := 0 to 3 do
  begin
    c := clusterColorIndex(y, m, i);
    idx := 2 * idx;
    if (c <> UCol) and (c <> DCol) then
      Inc(idx);
  end;
  Result := idx;
end;

procedure faceletCube.InvPhase1Brick256Coord(cc, x, y: integer);
var
  sc, m: integer;
begin
  m := (size - 1) div 2; // Index der mittleren Slice
  sc := cc mod 2; // Orientierung der L-y-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(y, m, 3, UCol);
    setClusterColorIndex(y, m, 3 + 4, DCol);
    setClusterColorIndex(y, m, 3 + 16, FCol);
    setClusterColorIndex(y, m, 1 + 20, BCol);
  end
  else
  begin
    setClusterColorIndex(y, m, 3, BCol);
    setClusterColorIndex(y, m, 3 + 4, FCol);
    setClusterColorIndex(y, m, 3 + 16, UCol);
    setClusterColorIndex(y, m, 1 + 20, DCol);
  end;
  sc := cc mod 2; // Orientierung der F-y-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(y, m, 2, UCol);
    setClusterColorIndex(y, m, 0 + 4, DCol);
    setClusterColorIndex(y, m, 1 + 12, LCol);
    setClusterColorIndex(y, m, 3 + 8, RCol);
  end
  else
  begin
    setClusterColorIndex(y, m, 2, LCol);
    setClusterColorIndex(y, m, 0 + 4, RCol);
    setClusterColorIndex(y, m, 1 + 12, DCol);
    setClusterColorIndex(y, m, 3 + 8, UCol);
  end;

  sc := cc mod 2; // Orientierung der R-y-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(y, m, 1, UCol);
    setClusterColorIndex(y, m, 1 + 4, DCol);
    setClusterColorIndex(y, m, 1 + 16, FCol);
    setClusterColorIndex(y, m, 3 + 20, BCol);
  end
  else
  begin
    setClusterColorIndex(y, m, 1, FCol);
    setClusterColorIndex(y, m, 1 + 4, BCol);
    setClusterColorIndex(y, m, 1 + 16, DCol);
    setClusterColorIndex(y, m, 3 + 20, UCol);
  end;

  sc := cc mod 2; // Orientierung der B-y-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(y, m, 0, UCol);
    setClusterColorIndex(y, m, 2 + 4, DCol);
    setClusterColorIndex(y, m, 1 + 8, RCol);
    setClusterColorIndex(y, m, 3 + 12, LCol);
  end
  else
  begin
    setClusterColorIndex(y, m, 0, RCol);
    setClusterColorIndex(y, m, 2 + 4, LCol);
    setClusterColorIndex(y, m, 1 + 8, DCol);
    setClusterColorIndex(y, m, 3 + 12, UCol);
  end;

  sc := cc mod 2; // Orientierung der L-x-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(x, m, 3, UCol);
    setClusterColorIndex(x, m, 3 + 4, DCol);
    setClusterColorIndex(x, m, 3 + 16, FCol);
    setClusterColorIndex(x, m, 1 + 20, BCol);
  end
  else
  begin
    setClusterColorIndex(x, m, 3, BCol);
    setClusterColorIndex(x, m, 3 + 4, FCol);
    setClusterColorIndex(x, m, 3 + 16, UCol);
    setClusterColorIndex(x, m, 1 + 20, DCol);
  end;
  sc := cc mod 2; // Orientierung der F-x-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(x, m, 2, UCol);
    setClusterColorIndex(x, m, 0 + 4, DCol);
    setClusterColorIndex(x, m, 1 + 12, LCol);
    setClusterColorIndex(x, m, 3 + 8, RCol);
  end
  else
  begin
    setClusterColorIndex(x, m, 2, LCol);
    setClusterColorIndex(x, m, 0 + 4, RCol);
    setClusterColorIndex(x, m, 1 + 12, DCol);
    setClusterColorIndex(x, m, 3 + 8, UCol);
  end;

  sc := cc mod 2; // Orientierung der R-x-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(x, m, 1, UCol);
    setClusterColorIndex(x, m, 1 + 4, DCol);
    setClusterColorIndex(x, m, 1 + 16, FCol);
    setClusterColorIndex(x, m, 3 + 20, BCol);
  end
  else
  begin
    setClusterColorIndex(x, m, 1, FCol);
    setClusterColorIndex(x, m, 1 + 4, BCol);
    setClusterColorIndex(x, m, 1 + 16, DCol);
    setClusterColorIndex(x, m, 3 + 20, UCol);
  end;

  sc := cc mod 2; // Orientierung der B-x-Slice
  if sc = 0 then
  begin
    setClusterColorIndex(x, m, 0, UCol);
    setClusterColorIndex(x, m, 2 + 4, DCol);
    setClusterColorIndex(x, m, 1 + 8, RCol);
    setClusterColorIndex(x, m, 3 + 12, LCol);
  end
  else
  begin
    setClusterColorIndex(x, m, 0, RCol);
    setClusterColorIndex(x, m, 2 + 4, LCol);
    setClusterColorIndex(x, m, 1 + 8, DCol);
    setClusterColorIndex(x, m, 3 + 12, UCol);
  end;

end;


//for large odd sized cubes. The x-bricks UD color position is defined by the cluster
//(x, size div 2), the y-bricks colors by cluster (y, size div 2)

//function faceletCube.getUDBrickXYDepth(x, y: integer): integer;
//var
//  i, bx, bx_class, bx_sym, bx1, bx1_class, bx1_sym, by, by1, byt, depth_mod3: integer;
//  m: Moves;
//begin
//  bx := Phase1BrickCoord(x);
//  bx_class := UDBrickCoordToSymCoord[bx].c_idx;
//  bx_sym := UDBrickCoordToSymCoord[bx].sym;
//  i := 0;//find one of the bitwise coded symmetries
//  while (bx_sym and (1 shl i)) = 0 do
//    Inc(i);
//  bx_sym := i;
//  by := Phase1BrickCoord(y);
//  by1 := UDBrickCoordSymTransform[by, bx_sym];
//  depth_mod3 := get_bxby_depth3(bx_class, by1);

//  Result := 0;
//  while (bx <> 0) or (by <> 0) do
//  begin
//    if depth_mod3 = 0 then
//      depth_mod3 := 3;
//    for m := fU1 to yB3 do
//    begin
//      case m of
//        fU1..fB3:
//        begin//face moves move all bricks
//          bx1 := UDCenterMove[bx, Ord(m)];
//          by1 := UDCenterMove[by, Ord(m)];
//        end;
//        xU1..xB3:
//        begin//x-moves move only x-bricks
//          bx1 := UDCenterMove[bx, Ord(m)];
//          by1 := by;
//        end;
//        yU1..yB3:
//        begin//y-moves move only y-bricks. Caveat: it is essentially
//          // an x move of a different orbit!
//          bx1 := bx;
//          by1 := UDCenterMove[by, Ord(m) - 18];
//        end;
//      end;
//      bx1_class := UDBrickCoordToSymCoord[bx1].c_idx;
//      bx1_sym := UDBrickCoordToSymCoord[bx1].sym;
//      i := 0;//find one bitwise coded symmetry
//      while (bx1_sym and (1 shl i)) = 0 do
//        Inc(i);
//      bx1_sym := i;
//      byt := UDBrickCoordSymTransform[by1, bx1_sym];
//      if get_bxby_depth3(bx1_class, byt) = depth_mod3 - 1 then
//      begin
//        //        Form1.Memo1.Lines.Add(Format('%s', [MoveStrings[m]]));

//        Inc(Result);
//        bx := bx1;
//        by := by1;
//        Dec(depth_mod3);
//        break;
//      end;
//    end;
//  end;
//end;

//function faceletCube.getUDBrickXCentXDepth(x, y: integer): integer;
//var
//  i, bx, bx_class, bx_sym, bx1, bx1_class, bx1_sym, by, by1, byt, depth_mod3: integer;
//  m: Moves;
//begin
//  bx := Phase1BrickCoord(x);
//  bx_class := UDBrickCoordToSymCoord[bx].c_idx;
//  bx_sym := UDBrickCoordToSymCoord[bx].sym;
//  i := 0;//find one of the bitwise coded symmetries
//  while (bx_sym and (1 shl i)) = 0 do
//    Inc(i);
//  bx_sym := i;
//  by := Phase1CenterCoord(x, y);
//  by1 := UDBrickCoordSymTransform[by, bx_sym];
//  depth_mod3 := get_bxcx_depth3(bx_class, by1);

//  Result := 0;
//  while (bx <> 0) or (by <> 0) do
//  begin
//    if depth_mod3 = 0 then
//      depth_mod3 := 3;
//    for m := fU1 to yB3 do
//    begin
//      case m of
//        fU1..fB3:
//        begin//face moves move x-bricks and (x,y)-orbit centers
//          bx1 := UDCenterMove[bx, Ord(m)];
//          by1 := UDCenterMove[by, Ord(m)];
//        end;
//        xU1..xB3:
//        begin//x-moves move x-bricks and (x,y)-orbit centers
//          bx1 := UDCenterMove[bx, Ord(m)];
//          by1 := UDCenterMove[by, Ord(m)];
//        end;
//        yU1..yB3:
//        begin//y-moves move only (x,y)-orbit centers
//          bx1 := bx;
//          by1 := UDCenterMove[by, Ord(m)];
//        end;
//      end;
//      bx1_class := UDBrickCoordToSymCoord[bx1].c_idx;
//      bx1_sym := UDBrickCoordToSymCoord[bx1].sym;
//      i := 0;//find one bitwise coded symmetry
//      while (bx1_sym and (1 shl i)) = 0 do
//        Inc(i);
//      bx1_sym := i;
//      byt := UDBrickCoordSymTransform[by1, bx1_sym];
//      if get_bxcx_depth3(bx1_class, byt) = depth_mod3 - 1 then
//      begin
//        //Form1.Memo1.Lines.Add(Format('%s', [MoveStrings[m]]));

//        Inc(Result);
//        bx := bx1;
//        by := by1;
//        Dec(depth_mod3);
//        break;
//      end;
//    end;
//  end;
//end;




function faceletCube.Phase1CenterCoord(x, y: integer): integer;
  //UD-Centers coordinate of clusters (x,y)
  // solved position has index 0
var
  occupied: array [0 .. 23] of boolean;
  c: ColorIndex;
  i, k, n: integer;
begin
  for i := 0 to 23 do
  begin
    c := clusterColorIndex(x, y, i);
    if (c = UCol) or (c = DCol) then
      occupied[i] := True
    else
      occupied[i] := False;
  end;

  Result := 0;
  n := 0;
  k := 1;

  while k <= 8 do  //8 positions of the UD cluster are occupied
  begin
    if occupied[n] then
    begin
      Inc(Result, Cnk(n, k));
      Inc(k);
    end;
    Inc(n);
  end;
end;

procedure faceletCube.InvPhase1CenterCoord(cc, x, y: integer);
var
  n, k, v, c: integer;
  occupied: array [0 .. 23] of boolean;
  col, setCol: ColorIndex;
begin
  for n := 0 to 23 do
    occupied[n] := False;
  n := 23;
  k := 8;//8 positions are set
  while k >= 1 do
  begin
    v := Cnk(n, k);
    if cc >= v then
    begin
      occupied[n] := True;
      Dec(cc, v);
      Dec(k);
    end;
    Dec(n);
  end;

  n := 0;
  setCol := UCol;
  for c := 0 to 23 do
    if occupied[c] then
    begin
      col := clusterColorIndex(x, y, c);
      if col <> setCol then
      begin
        k := 0;
        while (clusterColorIndex(x, y, k) <> setCol) or
          (occupied[k] and (k < c) (* do not change already set *)) do
          Inc(k);
        setClusterColorIndex(x, y, c, setCol);
        setClusterColorIndex(x, y, k, col);
      end;
      Inc(n);
      if n = 4 then // use DCol for n>=4
        setCol := DCol;
    end;
end;


procedure faceletCube.printMoves(i, j: integer);
var
  mm, s: string;
  k: integer;
begin
  mm := '';
  for k := 0 to mvIdx - 1 do
  begin
    s := moveStrings[fxymoves[k]];
    s := StringReplace(s, '1', '', []);
    s := StringReplace(s, '3', '''', []);
    s := StringReplace(s, 'x', IntToStr(i + 1), []);
    s := StringReplace(s, 'y', IntToStr(j + 1), []);
    s := StringReplace(s, 'f', '', []);
    mm := mm + ' ' + s;
  end;
  mm := mm + ' (' + IntToStr(mvIdx) + ')';
  Form1.Memo1.Lines.Add('(' + IntToStr(i) + ',' + IntToStr(j) + '):' + mm);
end;

procedure faceletCube.applyMoves(i, j: integer);
var
  mv: moves;
  k, n, slc, pow: integer;
  a: Axis;
begin
  for k := 0 to mvIdx - 1 do
  begin
    mv := fxymoves[k];
    if mv < xU1 then
      slc := 0
    else if mv < yU1 then
      slc := i
    else
      slc := j;
    n := Ord(mv) mod 18;
    a := Axis(n div 3);
    pow := n mod 3;
    for n := 0 to pow do
      move(a, slc);
  end;
end;

procedure faceletCube.applyMovesEdges;
var
  mv: moves;
  i,k: integer;
begin
  for k := 0 to mvIdx - 1 do
  begin
    mv := fxymoves[k];
    for i:= 1 to edgemx-1 do
    edgemove(i,mv);
  end;
end;






function faceletCube.MakeUDXCross(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;

  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchUDXCross(Phase1CenterCoord(x, x), Phase1Brick256Coord(x, x) and $F, togo);
    Inc(togo);
  end;
  Result := True;
end;

function faceletCube.MakeUDPlusCross1(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchUDPlusCross1(Phase1CenterCoord(x, size div 2), togo);
    // 0 ist die slicecoord der schon gelösten cluster.
    // diese dürfen ja nicht wieder zerstört werden
    Inc(togo);
  end;
  Result := True;
end;

procedure faceletCube.SearchUDPlusCross1(cc, togo: integer);
var
  mv: moves;
  newcc: integer;
begin
  if (UDPlusCross1Prun[cc] > togo) then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;
    while True do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase1[NoMove, mv]
      else
        mv := nextMovePhase1[fxymoves[mvIdx - 1], mv];

      if mv = NoMove then
      begin
        Exit;
      end
      else
      begin
        if mv < yU1 then
          newcc := UDCenterMove[cc, Ord(mv)]
        else
          continue;

        if (newcc = cc) then
          continue;

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchUDPlusCross1(newcc, togo - 1);

        if found then
          // kehre zurück, ohne mvIdx zu verändern
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;

procedure faceletCube.SearchUDXCross(ccx, slx, togo: integer);
var
  mv: moves;
begin

  if UDXCrossPrun[B_24_8 * slx + ccx] > togo then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;
    while True do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase1[NoMove, mv]
      else
        mv := nextMovePhase1[fxymoves[mvIdx - 1], mv];
      if (mv < xU1) and not UDfaceMoveAllowed[slx, Ord(mv)] then
        continue;
      if mv > xB3 then // no y-moves used
      begin
        Exit;
      end
      else
      begin
        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        if mv < xU1 then
          SearchUDXCross(UDXCrossMove[ccx, Ord(mv)], slx, togo - 1)
        else
          SearchUDXCross(UDXCrossMove[ccx, Ord(mv)],
            UDBrick256Move[slx, Ord(mv) + 18], togo - 1);
        if found then
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;

end;

function faceletCube.nextMovePh2(idx: integer; currMove: moves): moves;
var
  pm: moves;
begin
  if currMove = yB3 then // done
    Exit(NoMove);
  if idx = 0 then
  begin
    if currMove = InitMove then
    begin
      Exit(fR2); // U,D moves useless
    end
    else
    begin
      while not Phase2Allowed[Ord(Succ(currMove))] do
        Inc(currMove);
      Exit(Succ(currMove));
    end;
  end

  else
  begin
    pm := fxymoves[idx - 1]; // predecessor move
    while True do
    begin
      while not Phase2Allowed[Ord(Succ(currMove))] do
        Inc(currMove);
      currMove := Succ(currMove);
      if currMove = NoMove then
        Exit(NoMove);

      if Ord(pm) < Ord(xU1) then //previous move is face move
      begin
        if (Ord(currMove) <= Ord(pm)) then
          //all face moves commute restricted to the centers
          continue;
        if Ord(currMove) >= Ord(xU1) then //face move followed by slice move
          // always valid
          Exit(currMove)
        else // pm<currmove<xU1
        begin
          if Ord(pm) div 3 = Ord(currMove) div 3 then
            // same face
            continue
          else
            Exit(currMove);
        end;
      end;

      //Ord(pm) >= Ord(xU1), previous move is slice move
      if (Ord(pm) div 6) mod 3 <> (Ord(currMove) div 6) mod 3 then
        Exit(currMove);
      // pm and currMove are on different axes and hence do not commute


      // both moves are on the same axis and commute
      // we can force an order
      if Ord(currMove) <= Ord(pm) then
        continue;

      // if the  prefixes f,x,y are different for both moves, currmove is valid
      if Ord(currMove) div 18 <> Ord(pm) div 18 then
        Exit(currMove);

      // we have the same prefix and the same axis
      if (Ord(currMove) mod 6) div 3 <> (Ord(pm) mod 6) div 3 then
        // moves are on different slices of the axis
        Exit(currMove)
      else
        continue;
    end;
  end;
end;

// coordinate of FB-centers of cluster (x,y)
// phase 1 has to be finished already!
function faceletCube.Phase2CenterCoord(x, y: integer): integer;
var
  occupied: array [0 .. 15] of boolean;
  c: ColorIndex;
  i, s, k, n: integer;
begin
  for i := 0 to 15 do
  begin
    c := clusterColorIndex(x, y, i + 8); // +8 because we start with R-face
    if (c = FCol) or (c = BCol) then
      occupied[i] := True
    else
      occupied[i] := False;
  end;

  s := 0;
  k := 7; // 8 cubies
  n := 15; // on 16 Positionen
  while k >= 0 do
  begin
    if occupied[n] then
      Dec(k)
    else
      s := s + Cnk(n, k);
    Dec(n);
  end;
  Result := s; // ID is 0
end;

procedure faceletCube.InvPhase2CenterCoord(cc, x, y: integer);
var
  n, k, v, c: integer;
  occupied: array [0 .. 15] of boolean;
  col, setCol: ColorIndex;
begin
  for n := 0 to 15 do
    occupied[n] := False;
  n := 15;
  k := 7;
  while k >= 0 do
  begin
    v := Cnk(n, k);
    if cc < v then
    begin
      Dec(k);
      occupied[n] := True;
    end
    else
      Dec(cc, v);
    Dec(n);
  end;

  n := 0;
  setCol := FCol;
  for c := 0 to 15 do
    if occupied[c] then
    begin
      col := clusterColorIndex(x, y, c + 8);
      if col <> setCol then
      begin
        k := 0;
        while (clusterColorIndex(x, y, k + 8) <> setCol) or
          (occupied[k] and (k < c) (* do not change already set *)) do
          Inc(k);
        setClusterColorIndex(x, y, c + 8, setCol);
        setClusterColorIndex(x, y, k + 8, col);
      end;
      Inc(n);
      if n = 4 then // use DBCol now
        setCol := BCol;
    end;
end;


function faceletCube.MakeFBPlusCross(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchFBPlusCross(Phase2CenterCoord(x, size div 2), edgeparity(x), togo);
    Inc(togo);
  end;
  Result := True;
end;

procedure faceletCube.SearchFBPlusCross(cc, ep, togo: integer);
var
  mv: moves;
  newcc, newep: integer;
begin
  if (FBPlusCrossPrun[2 * cc + ep] > togo) then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;
    while True do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase2Arr[NoMove, mv]
      else
        mv := nextMovePhase2Arr[fxymoves[mvIdx - 1], mv];

      if mv = NoMove then
      begin
        Exit;
      end
      else
      begin

        if mv < yU1 then
        begin
          newcc := FBCenterMove[cc, Ord(mv)];
          if (mv = xU1) or (mv = xU3) or (mv = xD1) or (mv = xD3) then
            newep := 1 - ep
          else
            newep := ep;
        end

        else
          continue;//no y-moves for +cluster

        if (newcc = cc) and (newep = ep) then
          continue;

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchFBPlusCross(newcc, newep, togo - 1);

        if found then
          // return without changing mvIdx
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;

// x<>y und +cross muss schon gesetzt sein!
function faceletCube.Phase2SliceCoord(x, y: integer): integer;
var
  m, idx: integer;
  c: ColorIndex;
begin
  m := (size - 1) div 2; // Index der mittleren Slice
  idx := 0;

  // Es reicht die Positionen von F zu betrachten
  c := clusterColorIndex(x, m, 16); // xU-slice
  idx := 2 * idx;
  if (c <> FCol) and (c <> BCol) then
    Inc(idx);
  c := clusterColorIndex(x, m, 18); // xD-slice
  idx := 2 * idx;
  if (c <> FCol) and (c <> BCol) then
    Inc(idx);

  c := clusterColorIndex(y, m, 16); // yU-slice
  idx := 2 * idx;
  if (c <> FCol) and (c <> BCol) then
    Inc(idx);
  c := clusterColorIndex(y, m, 18); // yD-slice
  idx := 2 * idx;
  if (c <> FCol) and (c <> BCol) then
    Inc(idx);
  Result := idx;
end;


procedure faceletCube.InvPhase2SliceCoord(cc, x, y: integer);
var
  sc, m: integer;
begin

  m := (size - 1) div 2;
  sc := cc mod 2; // Orientierung der yD-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(y, m, 18, FCol);
    setClusterColorIndex(y, m, 14, LCol);
    setClusterColorIndex(y, m, 22, BCol);
    setClusterColorIndex(y, m, 10, RCol);
  end
  else
  begin
    setClusterColorIndex(y, m, 18, LCol);
    setClusterColorIndex(y, m, 14, BCol);
    setClusterColorIndex(y, m, 22, RCol);
    setClusterColorIndex(y, m, 10, FCol);
  end;
  sc := cc mod 2; // Orientierung der yU-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(y, m, 16, FCol);
    setClusterColorIndex(y, m, 12, LCol);
    setClusterColorIndex(y, m, 20, BCol);
    setClusterColorIndex(y, m, 8, RCol);
  end
  else
  begin
    setClusterColorIndex(y, m, 16, LCol);
    setClusterColorIndex(y, m, 12, BCol);
    setClusterColorIndex(y, m, 20, RCol);
    setClusterColorIndex(y, m, 8, FCol);
  end;

  sc := cc mod 2; // Orientierung der xD-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(x, m, 18, FCol);
    setClusterColorIndex(x, m, 14, LCol);
    setClusterColorIndex(x, m, 22, BCol);
    setClusterColorIndex(x, m, 10, RCol);
  end
  else
  begin
    setClusterColorIndex(x, m, 18, LCol);
    setClusterColorIndex(x, m, 14, BCol);
    setClusterColorIndex(x, m, 22, RCol);
    setClusterColorIndex(x, m, 10, FCol);
  end;

  sc := cc mod 2; // Orientierung der xU-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(x, m, 16, FCol);
    setClusterColorIndex(x, m, 12, LCol);
    setClusterColorIndex(x, m, 20, BCol);
    setClusterColorIndex(x, m, 8, RCol);
  end
  else
  begin
    setClusterColorIndex(x, m, 16, LCol);
    setClusterColorIndex(x, m, 12, BCol);
    setClusterColorIndex(x, m, 20, RCol);
    setClusterColorIndex(x, m, 8, FCol);
  end;
end;

procedure faceletCube.SearchFBFullCenter(ccx, slx, ccy, togo: integer);
var
  mv: moves;

  newccx, newccy, newslx: integer;

begin

  Application.ProcessMessages;

  if (FBFullCenterSlicePrun[UInt64(B_16_8) * (UInt64(B_16_8) * slx + ccx) +
    ccy] > togo) then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;

    while True do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase2Arr[NoMove, mv]
      else
        mv := nextMovePhase2Arr[fxymoves[mvIdx - 1], mv];
      if (mv < xU1) and not FBfaceMoveAllowed[slx, Ord(mv)] then
        continue;

      if mv = NoMove then
      begin
        Exit;
      end
      else
      begin
        case mv of
          fU1..FB3:
          begin
            newccx := FBCenterMove[ccx, Ord(mv)];
            newccy := FBCenterMove[ccy, Ord(mv)];
            newslx := slx;
          end;
          xU1..xB3:
          begin
            newccx := FBCenterMove[ccx, Ord(mv)];
            newccy := FBCenterMove[ccy, Ord(mv) + 18];
            newslx := FBSliceMove[slx, Ord(mv)];
          end;
          yU1..yB3:
          begin
            newccx := FBCenterMove[ccx, Ord(mv)];
            newccy := FBCenterMove[ccy, Ord(mv) - 18];
            newslx := FBSliceMove[slx, Ord(mv)];
          end;
        end;

        // do nothing if coordinates do not change
        if (newccx = ccx) and (newslx = slx) and (newccy = ccy) then
          continue;

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchFBFullCenter(newccx, newslx, newccy, togo - 1);

        if found then
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;


function faceletCube.MakeFBFullCenter(x, y: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchFBFullCenter(Phase2CenterCoord(x, y), Phase2SliceCoord(x, y),
      Phase2CenterCoord(y, x), togo);
    Inc(togo);
  end;
  Result := True;
end;

procedure faceletCube.SearchFBXCross(ccx, sly, togo: integer);
var
  mv: moves;
begin

  Application.ProcessMessages;
  if FBXCrossPrun[B_16_8 * sly + ccx] > togo then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;
    while True do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase2Arr[NoMove, mv]
      else
        mv := nextMovePhase2Arr[fxymoves[mvIdx - 1], mv];
      if (mv < xU1) and not FBfaceMoveAllowed[sly, Ord(mv)] then
        continue;
      if mv > xB3 then
      begin
        Exit;
      end
      else
      begin

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        if mv < xU1 then
          SearchFBXCross(FBXCrossMove[ccx, Ord(mv)], sly, togo - 1)
        else
          SearchFBXCross(FBXCrossMove[ccx, Ord(mv)],
            FBSliceMove[sly, Ord(mv) + 18], togo - 1);
        // y-Koordinate hat die beiden low bits
        if found then
          // kehre zurück, ohne mvIdx zu verändern
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;

end;

function faceletCube.MakeFBXCross(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;

  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchFBXCross(Phase2CenterCoord(x, x), Phase2SliceCoord(x, x) and $3, togo);
    // $3 um die y-slice coordinate zu erhalten, die die hinteren Bits enthält
    Inc(togo);
  end;
  Result := True;

end;

/// ////////////////////////// Phase 3 /////////////////////////////////////////


function faceletCube.nextMovePh3(idx: integer; currMove: moves): moves;
var
  pm: moves;
begin
  if currMove = yB3 then // done
    Exit(NoMove);
  if idx = 0 then
  begin
    while not Phase3Allowed[Ord(Succ(currMove))] do
      Inc(currMove);
    Exit(Succ(currMove));
  end
  else
  begin
    pm := fxymoves[idx - 1]; // predecessor move
    while True do
    begin
      while not Phase3Allowed[Ord(Succ(currMove))] do
        Inc(currMove);
      currMove := Succ(currMove);
      if currMove = NoMove then
        Exit(NoMove);

      if Ord(pm) < Ord(xU1) then //previous move is face move
      begin
        if (Ord(currMove) <= Ord(pm)) then
          //all face moves commute restricted to the centers
          continue;
        if Ord(currMove) >= Ord(xU1) then //face move followed by slice move
          // always valid
          Exit(currMove)
        else // pm<currmove<xU1
        begin
          if Ord(pm) div 3 = Ord(currMove) div 3 then
            // same face
            continue
          else
            Exit(currMove);
        end;
      end;

      //Ord(pm) >= Ord(xU1), previous move is slice move

      if (Ord(pm) div 18 = Ord(currmove) div 18) and (Ord(currMove) <= Ord(pm)) then
        continue;
      // all x-slice moves commute for  (x,y)-orbit with x<>y
      // all y-slice moves commute for  (x,y)-orbit with x<>y

      if (Ord(pm) div 6) mod 3 <> (Ord(currMove) div 6) mod 3 then
        Exit(currMove);
      // pm and currMove are on different axes and hence do not commute


      // both moves are on the same axis and commute
      // we can force an order
      if Ord(currMove) <= Ord(pm) then
        continue;

      // if the  prefixes f,x,y are different for both moves, currmove is valid
      if Ord(currMove) div 18 <> Ord(pm) div 18 then
        Exit(currMove);

      // we have the same prefix and the same axis
      if (Ord(currMove) mod 6) div 3 <> (Ord(pm) mod 6) div 3 then
        // moves are on different slices of the axis
        Exit(currMove)
      else
        continue;
    end;
  end;
end;

function faceletCube.Ph3RLFBCenterCoord(x, y: integer): UInt16;
  //0<=cc<B_8_4^2
begin
  Result := 70 * Phase3CenterCoord(x, y, R) + Phase3CenterCoord(x, y, F);
end;

procedure faceletCube.InvPh3RLFBCenterCoord(cc, x, y: integer);
begin
  InvPhase3CenterCoord(cc mod 70, x, y, F);
  InvPhase3CenterCoord(cc div 70, x, y, R);
end;


function faceletcube.Ph3Brick702Coord(x: integer): integer;
  //0<=bc<B_8_4^2
begin
  Result := B_8_4 * Phase3CenterCoord(x, size div 2, R) +
    Phase3CenterCoord(x, size div 2, F);
end;

procedure faceletcube.InvPh3Brick702Coord(br, x: integer);

begin
  InvPhase3CenterCoord(br div B_8_4, x, size div 2, R);
  InvPhase3CenterCoord(br mod B_8_4, x, size div 2, F);
end;



// in phase 3 a (x,y) cluster separates into 3 subclusters of size 8
//one for each direction of the cube
function faceletCube.Phase3CenterCoord(x, y: integer; a: Axis): integer;
var
  occupied: array [0 .. 7] of boolean;
  c, centc: ColorIndex;
  i, s, k, n: integer;
begin
  centc := faceCols[Ord(a), size div 2, size div 2]; //center color
  for i := 0 to 7 do
  begin
    c := clusterColorIndex(x, y, i + 4 * Ord(a));
    if c = centc then
      occupied[i] := True
    else
      occupied[i] := False;
  end;

  s := 0;
  k := 3; // 4 Cubies
  n := 7; // auf 8 Positionen
  while k >= 0 do
  begin
    if occupied[n] then
      Dec(k)
    else
      s := s + Cnk(n, k);
    Dec(n);
  end;
  Result := 69 - s; // ID soll 0 sein
end;

//axis 0,2,4 are used
procedure faceletCube.InvPhase3CenterCoord(cc, x, y: integer; a: Axis);
var
  n, k, v, c: integer;
  occupied: array [0 .. 7] of boolean;
  col, setCol: ColorIndex;
begin
  cc := 69 - cc;
  for n := 0 to 7 do
    occupied[n] := False;
  n := 7;
  k := 3;
  while k >= 0 do
  begin
    v := Cnk(n, k);
    if cc < v then
    begin
      Dec(k);
      occupied[n] := True;
    end
    else
      Dec(cc, v);
    Dec(n);
  end;

  n := 0;
  setCol := faceCols[Ord(a), size div 2, size div 2]; //center color
  for c := 0 to 7 do
    if occupied[c] then
    begin
      col := clusterColorIndex(x, y, c + 4 * Ord(a));
      if col <> setCol then
      begin
        k := 0;
        while (clusterColorIndex(x, y, k + 4 * Ord(a)) <> setCol) or
          (occupied[k] and (k < c) (* do not change already set colors *)) do
          Inc(k);
        setClusterColorIndex(x, y, c + 4 * Ord(a), setCol);
        setClusterColorIndex(x, y, k + 4 * Ord(a), col);
      end;
      Inc(n);
      if n = 4 then
        setCol := faceCols[Ord(a) + 1, size div 2, size div 2]; //opposite center color
    end;
end;


function faceletCube.getPh3Brick702RLFBCentDepth(x, y: integer): integer;
var
  i, bx, bx_class, bx_sym, bx1, bx1_class, bx1_sym, by, by1, byt, cx,
  cx1, cxt, depth_mod3: integer;
  m: Moves;
begin
  bx := Ph3Brick702Coord(x);
  bx_class := Ph3Brick702CoordToSymCoord[bx].c_idx;
  bx_sym := Ph3Brick702CoordToSymCoord[bx].sym;
  i := 0;//find one of the bitwise coded symmetries
  while (bx_sym and (1 shl i)) = 0 do
    Inc(i);
  bx_sym := i;

  by := Ph3Brick702Coord(y);
  cx := Ph3RLFBCenterCoord(x, y);

  by1 := Ph3RLFBCentCoordSymTransform[by, bx_sym];
  cx1 := Ph3RLFBCentCoordSymTransform[cx, bx_sym];
  depth_mod3 := get_bycx_depth3(bx_class, 4900 * by1 + cx1);


  Result := 0;
  while (bx <> 0) or (by <> 0) or (cx <> 0) do
  begin
    if depth_mod3 = 0 then
      depth_mod3 := 3;

    for m := fU1 to yB3 do
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
      i := 0;//find one bitwise coded symmetry
      while (bx1_sym and (1 shl i)) = 0 do
        Inc(i);
      bx1_sym := i;

      byt := Ph3RLFBCentCoordSymTransform[by1, bx1_sym];
      cxt := Ph3RLFBCentCoordSymTransform[cx1, bx1_sym];
      if get_bycx_depth3(bx1_class, 4900 * byt + cxt) = depth_mod3 - 1 then
      begin
        //Form1.Memo1.Lines.Add(Format('%s', [MoveStrings[m]]));
        ///////////////////////////////////////////////////
        Inc(Result);
        bx := bx1;
        cx := cx1;
        by := by1;
        Dec(depth_mod3);
        break;
      end;
    end;
  end;
end;


function faceletCube.MakePh3RLFBPlusCross(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchPh3RLFBPlusCross(Ph3RLFBCenterCoord(x, size div 2), togo);
    Inc(togo);
  end;
  Result := True;
end;

procedure faceletCube.SearchPh3RLFBPlusCross(bx, togo: integer);
var
  mv: moves;
  bx1: UInt16;
begin
  if (Ph3RLFBPlusCrossPrun[bx] > togo) then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;
    while True do
    begin
      mv := Succ(mv);
      while not Phase3Allowed[Ord(mv)] do
        mv := Succ(mv);
      if mv = NoMove then
      begin
        Exit;
      end
      else
      begin
        if mv < yU1 then
        begin
          bx1 := Ph3RLFBCenterMove[bx, Ord(mv)];
        end
        else
          continue; // no y-moves

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchPh3RLFBPlusCross(bx1, togo - 1);

        if found then
          // return without changing mvIdx
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;


function faceletCube.MakePh3Cent702(x, y: integer): boolean;
var
  idx, togo, dx, dy: integer;
begin
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;

  dx := getPh3Brick702RLFBCentDepth(x, y);
  dy := getPh3Brick702RLFBCentDepth(y, x);
  togo := Max(dx, dy);
  while found = False do
  begin
    mvIdx := 0; // 1. empty place in  fxymoves
    //Form1.Memo1.Lines.Add(Format('Searching depth %d...', [togo]));
    SearchPh3Cent702(Ph3Brick702Coord(x), Ph3Brick702Coord(y),
      Ph3RLFBCenterCoord(x, y), Ph3RLFBCenterCoord(y, x), dx, dy, togo);
    Inc(togo);
  end;
  if found then
  begin
    Result := True;

  end
  else
    Result := False;
end;

procedure faceletCube.SearchPh3Cent702(bx, by, cx, cy, dx, dy, togo: integer);
var
  mv: moves;
  bx1, by1, bxt, byt, bx1_class, bx1_sym, by1_class, by1_sym: UInt16;
  cx1, cy1, cxt, cyt, dx1, dy1, i, bycx_distmod3, bxcy_distmod3: integer;

begin
  Application.ProcessMessages;
  //Form1.Memo1.Lines.Add(Format('bx: %d, togo: %d, mvidx: %d', [bx, togo, mvidx]));
  //printmoves(1,2);
  if stopProgram then
    exit;

  if (togo = 0) then
  begin
    found := True;
  end
  else
  begin
    mv := initmove;//we do not use fU1..fD3 moves      { TODO : das unbedingt testen }
    while True do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase3Arr[NoMove, mv]
      else
        mv := nextMovePhase3Arr[fxymoves[mvIdx - 1], mv];

      if mv < fR1 then
        continue; //No U or D moves necessary

      if mv = NoMove then
      begin
        Exit;
      end
      else
      begin

        case mv of
          fU1..fB3:
          begin
            bx1 := Ph3RLFBCenterMove[bx, Ord(mv)];
            cx1 := Ph3RLFBCenterMove[cx, Ord(mv)];
            by1 := Ph3RLFBCenterMove[by, Ord(mv)];
            cy1 := Ph3RLFBCenterMove[cy, Ord(mv)];
          end;
          xU1..xB3:
          begin
            bx1 := Ph3RLFBCenterMove[bx, Ord(mv)];
            cx1 := Ph3RLFBCenterMove[cx, Ord(mv)];
            by1 := by;// x moves move only x bricks
            cy1 := Ph3RLFBCenterMove[cy, Ord(mv) + 18];
          end;
          yU1..yB3:
          begin
            bx1 := bx;// y moves move only y bricks
            cx1 := Ph3RLFBCenterMove[cx, Ord(mv)];
            by1 := Ph3RLFBCenterMove[by, Ord(mv) - 18];
            cy1 := Ph3RLFBCenterMove[cy, Ord(mv) - 18];
          end;
        end;

        bx1_class := Ph3Brick702CoordToSymCoord[bx1].c_idx;
        bx1_sym := Ph3Brick702CoordToSymCoord[bx1].sym;
        i := 0;//find one bitwise coded symmetry
        while (bx1_sym and (1 shl i)) = 0 do
          Inc(i);
        bx1_sym := i;

        byt := Ph3RLFBCentCoordSymTransform[by1, bx1_sym];
        cxt := Ph3RLFBCentCoordSymTransform[cx1, bx1_sym];

        bycx_distmod3 := get_bycx_depth3(bx1_class, 4900 * byt + cxt);
        dx1 := distance[3 * dx + bycx_distmod3];
        if dx1 >= togo then
          continue;

        by1_class := Ph3Brick702CoordToSymCoord[by1].c_idx;
        by1_sym := Ph3Brick702CoordToSymCoord[by1].sym;
        i := 0;//find one bitwise coded symmetry
        while (by1_sym and (1 shl i)) = 0 do
          Inc(i);
        by1_sym := i;

        bxt := Ph3RLFBCentCoordSymTransform[bx1, by1_sym];
        cyt := Ph3RLFBCentCoordSymTransform[cy1, by1_sym];

        bxcy_distmod3 := get_bycx_depth3(by1_class, 4900 * bxt + cyt);
        dy1 := distance[3 * dy + bxcy_distmod3];
        if dy1 >= togo then
          continue;

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchPh3Cent702(bx1, by1, cx1, cy1, dx1, dy1, togo - 1);

        if found then
          // return without changing mvIdx
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;

end;


function faceletCube.MakePh3XCross(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  while found = False do
  begin
    mvIdx := 0; // 1. empty position in fxymoves
    SearchPh3XCross(Ph3Brick702Coord(x), Ph3RLFBCenterCoord(x, x), togo);
    Inc(togo);
  end;
  Result := True;
end;

procedure faceletCube.SearchPh3XCross(bx, cx, togo: integer);
var
  mv: moves;
  bx1, cx1: UInt16;
begin
  if (Ph3RLFBXCrossPrun[4900 * bx + cx] > togo) then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := fD3;//no U, D moves//InitMove;
    while True do
    begin
      mv := Succ(mv);
      while not Phase3Allowed[Ord(mv)] do
        mv := Succ(mv);
      if mv = NoMove then
      begin
        Exit;
      end
      else
      begin
        if mv < yU1 then
        begin
          bx1 := Ph3RLFBCenterMove[bx, Ord(mv)];
          cx1 := Ph3RLFBXCrossMove[cx, Ord(mv)];
        end
        else
          continue; // no y-moves

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchPh3XCross(bx1, cx1, togo - 1);

        if found then
          // return without changing mvIdx
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;

///////////////////////// phase 4 //////////////////////////////////////////////

function faceletCube.nextMovePh4(idx: integer; currMove: moves): moves;
var
  pm: moves;
begin
  if currMove = yB3 then // done
    Exit(NoMove);
  if idx = 0 then
  begin
    while not Phase4Allowed[Ord(Succ(currMove))] do
      Inc(currMove);
    Exit(Succ(currMove));
  end
  else
  begin
    pm := fxymoves[idx - 1]; // predecessor move
    while True do
    begin
      while not Phase4Allowed[Ord(Succ(currMove))] do
        Inc(currMove);
      currMove := Succ(currMove);
      if currMove = NoMove then
        Exit(NoMove);

      if Ord(pm) < Ord(xU1) then //previous move is face move
      begin
        if (Ord(currMove) <= Ord(pm)) then
          //all face moves commute restricted to the centers
          continue;
        if Ord(currMove) >= Ord(xU1) then //face move followed by slice move
          // always valid
          Exit(currMove)
        else // pm<currmove<xU1
        begin
          if Ord(pm) div 3 = Ord(currMove) div 3 then
            // same face
            continue
          else
            Exit(currMove);
        end;
      end;

      //Ord(pm) >= Ord(xU1), previous move is slice move

      if (Ord(pm) div 18 = Ord(currmove) div 18) and (Ord(currMove) <= Ord(pm)) then
        continue;
      // all x-slice moves commute for  (x,y)-orbit with x<>y becasue we ignore (x,x) centers
      // all y-slice moves commute for  (x,y)-orbit with x<>y

      if (Ord(pm) div 6) mod 3 <> (Ord(currMove) div 6) mod 3 then
        Exit(currMove);
      // pm and currMove are on different axes and hence do not commute


      // both moves are on the same axis and commute
      // we can force an order
      if Ord(currMove) <= Ord(pm) then
        continue;

      // if the  prefixes f,x,y are different for both moves, currmove is valid
      if Ord(currMove) div 18 <> Ord(pm) div 18 then
        Exit(currMove);

      // we have the same prefix and the same axis
      if (Ord(currMove) mod 6) div 3 <> (Ord(pm) mod 6) div 3 then
        // moves are on different slices of the axis
        Exit(currMove)
      else
        continue;
    end;
  end;
end;


procedure faceletCube.InvPhase4RLFBBrickCoord(cc, x, y: integer);
var
  m, i: integer;
begin

  m := size div 2;
  for i := 0 to 23 do
  begin
    setClusterColorIndex(x, m, i, NoCol);
    setClusterColorIndex(y, m, i, NoCol);
  end;


  if Odd(cc) then
    faceCols[Ord(b), m, y] := FCol
  else
    faceCols[Ord(F), m, size - 1 - y] := FCol;
  cc := cc div 2;
  if Odd(cc) then
    faceCols[Ord(b), m, size - 1 - y] := FCol
  else
    faceCols[Ord(F), m, y] := FCol;
  cc := cc div 2;

  if Odd(cc) then
    faceCols[Ord(L), m, y] := RCol
  else
    faceCols[Ord(R), m, size - 1 - y] := RCol;
  cc := cc div 2;
  if Odd(cc) then
    faceCols[Ord(L), m, size - 1 - y] := RCol
  else
    faceCols[Ord(R), m, y] := RCol;
  cc := cc div 2;

  if Odd(cc) then
    faceCols[Ord(b), m, x] := FCol
  else
    faceCols[Ord(F), m, size - 1 - x] := FCol;
  cc := cc div 2;
  if Odd(cc) then
    faceCols[Ord(b), m, size - 1 - x] := FCol
  else
    faceCols[Ord(F), m, x] := FCol;
  cc := cc div 2;

  if Odd(cc) then
    faceCols[Ord(L), m, x] := RCol
  else
    faceCols[Ord(R), m, size - 1 - x] := RCol;
  cc := cc div 2;
  if Odd(cc) then
    faceCols[Ord(L), m, size - 1 - x] := RCol
  else
    faceCols[Ord(R), m, x] := RCol;

end;


function faceletCube.Phase4RLFBBrickCoord(x, y: integer): integer;
  //0<=cc<256. if x-bricks are solved 0<=cc<16
var
  m: integer;
begin
  m := size div 2;
  Result := 0;
  Result := 2 * Result;
  if faceCols[Ord(R), m, x] <> RCol then
    Inc(Result);
  Result := 2 * Result;
  if faceCols[Ord(R), m, size - 1 - x] <> RCol then
    Inc(Result);
  Result := 2 * Result;
  if faceCols[Ord(F), m, x] <> FCol then
    Inc(Result);
  Result := 2 * Result;
  if faceCols[Ord(F), m, size - 1 - x] <> FCol then
    Inc(Result);

  Result := 2 * Result;
  if faceCols[Ord(R), m, y] <> RCol then
    Inc(Result);
  Result := 2 * Result;
  if faceCols[Ord(R), m, size - 1 - y] <> RCol then
    Inc(Result);
  Result := 2 * Result;
  if faceCols[Ord(F), m, y] <> FCol then
    Inc(Result);
  Result := 2 * Result;
  if faceCols[Ord(F), m, size - 1 - y] <> FCol then
    Inc(Result);
end;



function faceletCube.MakePh4UDPlusCross(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchPh4UDPlusCross(Phase3CenterCoord(x, size div 2, U), 0, togo);
    Inc(togo);
  end;
  Result := True;
end;


procedure faceletCube.SearchPh4UDPlusCross(c, b, togo: integer);
var
  mv: moves;
  b1, c1: UInt16;
begin
  if (Ph4UDPlusCrossPrun[16 * c + b] > togo) then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;
    while True do
    begin
      mv := Succ(mv);
      while not Phase4Allowed[Ord(mv)] do
        mv := Succ(mv);
      if mv > xB2 then //no y-moves
      begin
        Exit;
      end
      else
      begin

        case mv of
          fU1..fB3:
          begin
            b1 := b;//allowed face moves do not change this
            c1 := Phase4CenterMove[c, Ord(mv)];
          end;
          xU1..xB3:
          begin
            b1 := Phase4RLFBBrickMove[b, Ord(mv) + 18];
            // must use y-bricks internally for 0<=b<16
            c1 := Phase4CenterMove[c, Ord(mv)];
          end;
        end;

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchPh4UDPlusCross(c1, b1, togo - 1);
        if found then
          // return without changing mvIdx
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;

function faceletCube.Phase4UDBrickCoord(x, y: integer): integer;
begin
  Result := B_8_4 * Phase3CenterCoord(x, size div 2, U) +
    Phase3CenterCoord(y, size div 2, U);
end;

procedure faceletCube.InvPhase4UDBrickCoord(cc, x, y: integer);
begin
  InvPhase3CenterCoord(cc div B_8_4, x, size div 2, U);
  InvPhase3CenterCoord(cc mod B_8_4, y, size div 2, U);
end;



function faceletCube.MakePh4UDCenters(x, y: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  //Phase3CenterCoord(x, y, R)
  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchPh4UDCenter(Phase3CenterCoord(x, y, U),
      Phase3CenterCoord(y, x, U), Phase4UDBrickCoord(x, y),
      Phase4RLFBBrickCoord(x, y), togo);
    Inc(togo);
  end;
  Result := True;
end;


procedure faceletCube.SearchPh4UDCenter(cx, cy, bxy, bo, togo: integer);
var
  mv: moves;
  cx1, cy1, bxy1, bo1: integer;

begin
  Application.ProcessMessages;
  if Ph4UDCentBrickPrun[B_8_4 * (B_8_4 * bxy + cx) + cy] > togo then
    Exit;
  if (togo = 0) and (bo = 0) then { TODO : Bedingung relaxen, nur für 3. Seite nötig }
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;

    while True do
    begin

      if mvIdx = 0 then
        mv := nextMovePhase4Arr[NoMove, mv]
      else
        mv := nextMovePhase4Arr[fxymoves[mvIdx - 1], mv];

      if mv = NoMove then
      begin
        Exit;
      end
      else
      begin

        cx1 := Phase4CenterMove[cx, Ord(mv)];
        bxy1 := Phase4UDBrickMove[bxy, Ord(mv)];
        case mv of
          fU1..fB3:
          begin
            cy1 := Phase4CenterMove[cy, Ord(mv)];
            bo1 := bo;
          end;
          xU1..xB3:
          begin
            bo1 := Phase4RLFBBrickMove[bo, Ord(mv)];
            cy1 := Phase4CenterMove[cy, Ord(mv) + 18];
          end;
          yU1..yB3:
          begin
            bo1 := Phase4RLFBBrickMove[bo, Ord(mv)];
            cy1 := Phase4CenterMove[cy, Ord(mv) - 18];
          end;
        end;

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchPh4UDCenter(cx1, cy1, bxy1, bo1, togo - 1);

        if found then
          // kehre zurück, ohne mvIdx zu verändern
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;


function faceletCube.MakePh4XCross(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;

  while found = False do
  begin
    mvIdx := 0;
    SearchPh4XCross(Phase3CenterCoord(x, x, U),
      Phase4UDBrickCoord(x, x) mod B_8_4,
      { get only y-part}
      Phase4RLFBBrickCoord(x, x) and $F { get only y-part}, togo);
    Inc(togo);
  end;
  Result := True;
end;

procedure faceletCube.SearchPh4XCross(cx, b, bo, togo: integer);
var
  mv: moves;
  cx1, b1, bo1: UInt16;

begin
  Application.ProcessMessages;

  if Ph4UDXCrossPrun[B_8_4 * (B_8_4 * bo + b) + cx] > togo then
    Exit;
  if (togo = 0) then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;

    while True do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase4Arr[NoMove, mv]
      else
        mv := nextMovePhase4Arr[fxymoves[mvIdx - 1], mv];

      if mv > xB2 then
      begin
        Exit;
      end
      else
      begin
        cx1 := Phase4UDXCrossMove[cx, Ord(mv)];
        case mv of
          fU1..fB3:
          begin
            b1 := Phase4UDBrickMove[b, Ord(mv)];
            bo1 := bo;
          end;
          xU1..xB3:
          begin
            b1 := Phase4UDBrickMove[b, Ord(mv) + 18];
            bo1 := Phase4RLFBBrickMove[bo, Ord(mv) + 18];
          end;

        end;


        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchPh4XCross(cx1, b1, bo1, togo - 1);

        if found then
          // kehre zurück, ohne mvIdx zu verändern
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;

//phase 5
procedure faceletcube.edgemove(x: integer; mv: Moves);
//ecls must be initialized!
var
  tmp: integer;
begin
  case mv of
    fU1:
    begin
      tmp := ecls[x, 0];
      ecls[x, 0] := ecls[x, 3];
      ecls[x, 3] := ecls[x, 2];
      ecls[x, 2] := ecls[x, 1];
      ecls[x, 1] := tmp;
      tmp := ecls[x, 12];
      ecls[x, 12] := ecls[x, 22];
      ecls[x, 22] := ecls[x, 16];
      ecls[x, 16] := ecls[x, 20];
      ecls[x, 20] := tmp;

    end;
    fD1:
    begin
      tmp := ecls[x, 4];
      ecls[x, 4] := ecls[x, 7];
      ecls[x, 7] := ecls[x, 6];
      ecls[x, 6] := ecls[x, 5];
      ecls[x, 5] := tmp;
      tmp := ecls[x, 21];
      ecls[x, 21] := ecls[x, 18];
      ecls[x, 18] := ecls[x, 23];
      ecls[x, 23] := ecls[x, 14];
      ecls[x, 14] := tmp;
    end;
    fR1:
    begin
      tmp := ecls[x, 12];
      ecls[x, 12] := ecls[x, 15];
      ecls[x, 15] := ecls[x, 14];
      ecls[x, 14] := ecls[x, 13];
      ecls[x, 13] := tmp;
      tmp := ecls[x, 8];
      ecls[x, 8] := ecls[x, 5];
      ecls[x, 5] := ecls[x, 11];
      ecls[x, 11] := ecls[x, 1];
      ecls[x, 1] := tmp;
    end;
    fL1:
    begin
      tmp := ecls[x, 16];
      ecls[x, 16] := ecls[x, 19];
      ecls[x, 19] := ecls[x, 18];
      ecls[x, 18] := ecls[x, 17];
      ecls[x, 17] := tmp;
      tmp := ecls[x, 3];
      ecls[x, 3] := ecls[x, 10];
      ecls[x, 10] := ecls[x, 7];
      ecls[x, 7] := ecls[x, 9];
      ecls[x, 9] := tmp;
    end;
    fF1:
    begin
      tmp := ecls[x, 20];
      ecls[x, 20] := ecls[x, 9];
      ecls[x, 9] := ecls[x, 21];
      ecls[x, 21] := ecls[x, 8];
      ecls[x, 8] := tmp;
      tmp := ecls[x, 2];
      ecls[x, 2] := ecls[x, 17];
      ecls[x, 17] := ecls[x, 4];
      ecls[x, 4] := ecls[x, 15];
      ecls[x, 15] := tmp;
    end;
    fB1:
    begin
      tmp := ecls[x, 22];
      ecls[x, 22] := ecls[x, 11];
      ecls[x, 11] := ecls[x, 23];
      ecls[x, 23] := ecls[x, 10];
      ecls[x, 10] := tmp;
      tmp := ecls[x, 0];
      ecls[x, 0] := ecls[x, 13];
      ecls[x, 13] := ecls[x, 6];
      ecls[x, 6] := ecls[x, 19];
      ecls[x, 19] := tmp;
    end;

    fU2:
    begin
      edgemove(x, fU1);
      edgemove(x, fU1);
    end;
    fD2:
    begin
      edgemove(x, fD1);
      edgemove(x, fD1);
    end;
    fR2:
    begin
      edgemove(x, fR1);
      edgemove(x, fR1);
    end;
    fL2:
    begin
      edgemove(x, fL1);
      edgemove(x, fL1);
    end;
    fF2:
    begin
      edgemove(x, fF1);
      edgemove(x, fF1);
    end;
    fB2:
    begin
      edgemove(x, fB1);
      edgemove(x, fB1);
    end;

    fU3:
    begin
      edgemove(x, fU1);
      edgemove(x, fU1);
      edgemove(x, fU1);
    end;
    fD3:
    begin
      edgemove(x, fD1);
      edgemove(x, fD1);
      edgemove(x, fD1);
    end;
    fR3:
    begin
      edgemove(x, fR1);
      edgemove(x, fR1);
      edgemove(x, fR1);
    end;
    fL3:
    begin
      edgemove(x, fL1);
      edgemove(x, fL1);
      edgemove(x, fL1);
    end;
    fF3:
    begin
      edgemove(x, fF1);
      edgemove(x, fF1);
      edgemove(x, fF1);
    end;
    fB3:
    begin
      edgemove(x, fB1);
      edgemove(x, fB1);
      edgemove(x, fB1);
    end;

    xU1:
    begin
      tmp := ecls[x, 8];
      ecls[x, 8] := ecls[x, 13];
      ecls[x, 13] := ecls[x, 10];
      ecls[x, 10] := ecls[x, 17];
      ecls[x, 17] := tmp;
    end;
    xD1:
    begin
      tmp := ecls[x, 9];
      ecls[x, 9] := ecls[x, 19];
      ecls[x, 19] := ecls[x, 11];
      ecls[x, 11] := ecls[x, 15];
      ecls[x, 15] := tmp;
    end;
    xR1:
    begin
      tmp := ecls[x, 2];
      ecls[x, 2] := ecls[x, 21];
      ecls[x, 21] := ecls[x, 6];
      ecls[x, 6] := ecls[x, 22];
      ecls[x, 22] := tmp;
    end;
    xL1:
    begin
      tmp := ecls[x, 20];
      ecls[x, 20] := ecls[x, 0];
      ecls[x, 0] := ecls[x, 23];
      ecls[x, 23] := ecls[x, 4];
      ecls[x, 4] := tmp;
    end;
    xF1:
    begin
      tmp := ecls[x, 3];
      ecls[x, 3] := ecls[x, 18];
      ecls[x, 18] := ecls[x, 5];
      ecls[x, 5] := ecls[x, 12];
      ecls[x, 12] := tmp;
    end;
    xB1:
    begin
      tmp := ecls[x, 1];
      ecls[x, 1] := ecls[x, 14];
      ecls[x, 14] := ecls[x, 7];
      ecls[x, 7] := ecls[x, 16];
      ecls[x, 16] := tmp;
    end;

    xU2:
    begin
      edgemove(x, xU1);
      edgemove(x, xU1);
    end;
    xD2:
    begin
      edgemove(x, xD1);
      edgemove(x, xD1);
    end;
    xR2:
    begin
      edgemove(x, xR1);
      edgemove(x, xR1);
    end;
    xL2:
    begin
      edgemove(x, xL1);
      edgemove(x, xL1);
    end;
    xF2:
    begin
      edgemove(x, xF1);
      edgemove(x, xF1);
    end;
    xB2:
    begin
      edgemove(x, xB1);
      edgemove(x, xB1);
    end;

    xU3:
    begin
      edgemove(x, xU1);
      edgemove(x, xU1);
      edgemove(x, xU1);
    end;
    xD3:
    begin
      edgemove(x, xD1);
      edgemove(x, xD1);
      edgemove(x, xD1);
    end;
    xR3:
    begin
      edgemove(x, xR1);
      edgemove(x, xR1);
      edgemove(x, xR1);
    end;
    xL3:
    begin
      edgemove(x, xL1);
      edgemove(x, xL1);
      edgemove(x, xL1);
    end;
    xF3:
    begin
      edgemove(x, xF1);
      edgemove(x, xF1);
      edgemove(x, xF1);
    end;
    xB3:
    begin
      edgemove(x, xB1);
      edgemove(x, xB1);
      edgemove(x, xB1);
    end;
  end;
end;


procedure faceletcube.store(ed: Edge);
var
  i: integer;
begin
  case ed of
    FL:
    begin
      if badEdgeCnt(UR) <> 0 then
      begin
        fxymoves[mvIDx] := fF1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fF3;
        Inc(mvIDx);
      end
      else if badEdgeCnt(UL) <> 0 then
      begin
        fxymoves[mvIDx] := fF1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fF3;
        Inc(mvIDx);
      end
      else if badEdgeCnt(UF) <> 0 then
      begin
        fxymoves[mvIDx] := fL3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fL1;
        Inc(mvIDx);
      end
      else if badEdgeCnt(UB) <> 0 then
      begin
        fxymoves[mvIDx] := fL3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fL1;
        Inc(mvIDx);
      end

      else if badEdgeCnt(DR) <> 0 then
      begin
        fxymoves[mvIDx] := fF3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fF1;
        Inc(mvIDx);
      end
      else if badEdgeCnt(DL) <> 0 then
      begin
        fxymoves[mvIDx] := fF3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fF1;
        Inc(mvIDx);
      end
      else if badEdgeCnt(DF) <> 0 then
      begin
        fxymoves[mvIDx] := fL1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fL3;
        Inc(mvIDx);
      end
      else if badEdgeCnt(DB) <> 0 then
      begin
        fxymoves[mvIDx] := fL1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fL3;
        Inc(mvIDx);
      end;
    end;//FL
    BL:
    begin
      if badEdgeCnt(UF) <> 0 then
      begin
        fxymoves[mvIDx] := fL1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fL3;
        Inc(mvIDx);
      end
      else if badEdgeCnt(UB) <> 0 then
      begin
        fxymoves[mvIDx] := fL1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fL3;
        Inc(mvIDx);
      end
      else if badEdgeCnt(UL) <> 0 then
      begin
        fxymoves[mvIDx] := fB3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fB1;
        Inc(mvIDx);
      end
      else if badEdgeCnt(UR) <> 0 then
      begin
        fxymoves[mvIDx] := fB3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fB1;
        Inc(mvIDx);
      end

      else if badEdgeCnt(DF) <> 0 then
      begin
        fxymoves[mvIDx] := fL3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fL1;
        Inc(mvIDx);
      end
      else if badEdgeCnt(DB) <> 0 then
      begin
        fxymoves[mvIDx] := fL3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fL1;
        Inc(mvIDx);
      end
      else if badEdgeCnt(DL) <> 0 then
      begin
        fxymoves[mvIDx] := fB1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fB3;
        Inc(mvIDx);
      end
      else if badEdgeCnt(DR) <> 0 then
      begin
        fxymoves[mvIDx] := fB1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fB3;
        Inc(mvIDx);
      end;
    end;
    FR:
    begin
      if badEdgeCnt(UB) <> 0 then
      begin
        fxymoves[mvIDx] := fR1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fR3;
        Inc(mvIDx);
      end
      else if badEdgeCnt(UF) <> 0 then
      begin
        fxymoves[mvIDx] := fR1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fR3;
        Inc(mvIDx);
      end
      else if badEdgeCnt(UR) <> 0 then
      begin
        fxymoves[mvIDx] := fF3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fF1;
        Inc(mvIDx);
      end
      else if badEdgeCnt(UL) <> 0 then
      begin
        fxymoves[mvIDx] := fF3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fF1;
        Inc(mvIDx);
      end

      else if badEdgeCnt(DB) <> 0 then
      begin
        fxymoves[mvIDx] := fR3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fR1;
        Inc(mvIDx);
      end
      else if badEdgeCnt(DF) <> 0 then
      begin
        fxymoves[mvIDx] := fR3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fR1;
        Inc(mvIDx);
      end
      else if badEdgeCnt(DR) <> 0 then
      begin
        fxymoves[mvIDx] := fF1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fF3;
        Inc(mvIDx);
      end
      else if badEdgeCnt(DL) <> 0 then
      begin
        fxymoves[mvIDx] := fF1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fF3;
        Inc(mvIDx);
      end;
    end;
    BR:
    begin
      if badEdgeCnt(UL) <> 0 then
      begin
        fxymoves[mvIDx] := fB1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fB3;
        Inc(mvIDx);
      end
      else if badEdgeCnt(UR) <> 0 then
      begin
        fxymoves[mvIDx] := fB1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fB3;
        Inc(mvIDx);
      end
      else if badEdgeCnt(UB) <> 0 then
      begin
        fxymoves[mvIDx] := fR3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fR1;
        Inc(mvIDx);
      end
      else if badEdgeCnt(UF) <> 0 then
      begin
        fxymoves[mvIDx] := fR3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fU3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fR1;
        Inc(mvIDx);
      end

      else if badEdgeCnt(DL) <> 0 then
      begin
        fxymoves[mvIDx] := fB3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fB1;
        Inc(mvIDx);
      end
      else if badEdgeCnt(DR) <> 0 then
      begin
        fxymoves[mvIDx] := fB3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fB1;
        Inc(mvIDx);
      end
      else if badEdgeCnt(DB) <> 0 then
      begin
        fxymoves[mvIDx] := fR1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD3;
        Inc(mvIDx);
        fxymoves[mvIDx] := fR3;
        Inc(mvIDx);
      end
      else if badEdgeCnt(DF) <> 0 then
      begin
        fxymoves[mvIDx] := fR1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fD1;
        Inc(mvIDx);
        fxymoves[mvIDx] := fR3;
        Inc(mvIDx);
      end;
    end;//FL
  end;
end;

function faceletCube.MakeFLEdge:Boolean;
// fix the FL edge
var
  i, idx, togo, baded, freeslotU, freeslotD: integer;
  ed: Edge;
begin
  //for idx := Low(fxymoves) to High(fxymoves) do
  //  fxymoves[idx] := InitMove;
  mvIdx := 0;
  baded := 0;
  for ed := UR to DB do
    Inc(baded, badEdgeCnt(ed));
  if baded <> 0 then //some slot empty
  begin
    if badEdgeCnt(FL) = 0 then  //but if other edges ore ok save them first
    begin
      store(FL);
      applyMovesEdges;
      applyMoves(-1, -1); //only face moves
      printMoves(-1, -1);
      Result := True;
    end
    else if badEdgeCnt(FR) = 0 then
    begin
      store(FR);
      applyMovesEdges;
      applyMoves(-1, -1);
      printMoves(-1, -1);
      Result := True;
    end
    else if badEdgeCnt(BR) = 0 then
    begin
      store(BR);
      applyMovesEdges;
      applyMoves(-1, -1);
      printMoves(-1, -1);
      Result := True;
    end
    else if badEdgeCnt(BL) = 0 then
    begin
      store(BL);
       applyMovesEdges;
      applyMoves(-1, -1);
      printMoves(-1, -1);
      Result := True;
    end
    else
    begin
      result:=false;
    end;

  end
  else//last part of freeslice method
  begin
    result:=false;
  end;

end;

function faceletCube.badEdgeCnt(e: Edge): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to edgemx - 1 do
  begin
    if ecls[i, edgeidx[e, 0]] <> ecls[edgemx, edgeidx[e, 0]] then
      Inc(Result);
    if ecls[i, edgeidx[e, 1]] <> ecls[edgemx, edgeidx[e, 1]] then
      Inc(Result);
  end;
end;



procedure faceletCube.SearchFLEdge(x, sliceState, dist: byte; togo: integer);
var
  mv: moves;

begin
  Application.ProcessMessages;
  if dist > togo then
    exit;

  //if (togo = 0) then
  //begin
  //  found := True;
  //end
  //else
  //begin
  //  mv := InitMove;

  //  while True do
  //  begin
  //    if mvIdx = 0 then
  //      mv := nextMovePhase4Arr[NoMove, mv]
  //    else
  //      mv := nextMovePhase4Arr[fxymoves[mvIdx - 1], mv];

  //    if mv > xB2 then
  //    begin
  //      Exit;
  //    end
  //    else
  //    begin
  //      cx1 := Phase4UDXCrossMove[cx, Ord(mv)];
  //      case mv of
  //        fU1..fB3:
  //        begin
  //          b1 := Phase4UDBrickMove[b, Ord(mv)];
  //          bo1 := bo;
  //        end;
  //        xU1..xB3:
  //        begin
  //          b1 := Phase4UDBrickMove[b, Ord(mv) + 18];
  //          bo1 := Phase4RLFBBrickMove[bo, Ord(mv) + 18];
  //        end;

  //      end;


  //      fxymoves[mvIdx] := mv;
  //      Inc(mvIdx);
  //      SearchPh4XCross(cx1, b1, bo1, togo - 1);

  //      if found then
  //        // kehre zurück, ohne mvIdx zu verändern
  //        Exit;
  //      Dec(mvIdx);
  //    end;
  //  end;

  //end;
end;




//function faceletCube.SearchUDCent(x, y: integer): boolean;
//var
//  idx, togo, d1, d2,d3: integer;
//  i: integer;
//begin
//  found := False;
//  for idx := Low(fxymoves) to High(fxymoves) do
//    fxymoves[idx] := InitMove;

//  d1 := getUDBrickXCentXDepth(x, y);
//  d2 := getUDBrickXCentXDepth(y, x);
//  d3 := getUDBrickXYDepth(x, y);
//  togo := max(max(d1, d2),d3);
//  while found = False do
//  begin
//    mvIdx := 0; // 1. empty place in  fxymoves
//    Form1.Memo1.Lines.Add(Format('Searching depth %d...', [togo]));

//    SearchUDCenter(Phase1BrickCoord(x), Phase1CenterCoord(x, y),
//      Phase1BrickCoord(y), Phase1CenterCoord(y, x),
//      d3, d1, d2, togo);
//    Inc(togo);
//  end;
//  if found then
//  begin
//    Result := True;

//  end
//  else
//    Result := False;
//end;


////restores centers of orbits(x,y) and (y,x).
//procedure faceletCube.SearchUDCenter(bx, cx, by, cy, bxby_dist,
//  bxcx_dist, bycy_dist, togo: integer);
//var
//  mv: moves;
//  sc1: SymCoord32;
//  syms: UInt8;
//  i, bx_class, bx_sym, by_class, by_sym, altcx, altcy, altby,
//  bxcx_distmod3, bycy_distmod3, bxby_distmod3, bxcx_dist1, bycy_dist1,
//  bxby_dist1: integer;
//  bx1, by1, cx1, cy1: integer;

//begin
//  Application.ProcessMessages;
//   //Form1.Memo1.Lines.Add(Format('bx: %d, togo: %d, mvidx: %d', [bx, togo, mvidx]));
//   //printmoves(1,2);
//  if stopProgram then
//    exit;

//  if togo = 0 then
//  begin
//    found := True;
//  end
//  else
//  begin
//    mv := InitMove;
//    while True do
//    begin
//      if mvIdx = 0 then
//        mv := nextMovePhase1[NoMove, mv]
//      else
//        mv := nextMovePhase1[fxymoves[mvIdx - 1], mv];

//      if mv = NoMove then
//      begin
//        Exit;
//      end
//      else
//      begin
//        case mv of
//          fU1..fB3:
//          begin//face moves move x-bricks and (x,y)-orbit centers
//            bx1 := UDCenterMove[bx, Ord(mv)];
//            cx1 := UDCenterMove[cx, Ord(mv)];
//            by1 := UDCenterMove[by, Ord(mv)];
//            cy1 := UDCenterMove[cy, Ord(mv)];
//          end;
//          xU1..xB3:
//          begin
//            bx1 := UDCenterMove[bx, Ord(mv)];
//            cx1 := UDCenterMove[cx, Ord(mv)];
//            by1 := by;
//            // for (y,x) Orbit the roles of x-moves and y-moves are swapped
//            cy1 := UDCenterMove[cy, Ord(mv) + 18];
//          end;
//          yU1..yB3:
//          begin
//            bx1 := bx;
//            cx1 := UDCenterMove[cx, Ord(mv)];
//            by1 := UDCenterMove[by, Ord(mv) - 18];
//            cy1 := UDCenterMove[cy, Ord(mv) - 18];
//          end;
//        end;

//        bx_class := UDBrickCoordToSymCoord[bx1].c_idx;
//        bx_sym := UDBrickCoordToSymCoord[bx1].sym;
//        i := 0;//find one symmetry
//        while (bx_sym and (1 shl i)) = 0 do
//          Inc(i);
//        altcx := UDBrickCoordSymTransform[cx1, i];
//        altby := UDBrickCoordSymTransform[by1, i]; //compute here
//        bxcx_distmod3 := get_bxcx_depth3(bx_class, altcx);
//        bxcx_dist1 := distance[3 * bxcx_dist + bxcx_distmod3];
//         if bxcx_dist1 >= togo then
//          continue;

//        by_class := UDBrickCoordToSymCoord[by1].c_idx;
//        by_sym := UDBrickCoordToSymCoord[by1].sym;
//        i := 0;//find one symmetry
//        while (by_sym and (1 shl i)) = 0 do
//          Inc(i);
//        altcy := UDBrickCoordSymTransform[cy1, i];
//        bycy_distmod3 := get_bxcx_depth3(by_class, altcy);
//        bycy_dist1 := distance[3 * bycy_dist + bycy_distmod3];
//        if bycy_dist1 >= togo then
//          continue;

//        bxby_distmod3 := get_bxby_depth3(bx_class, altby);
//        bxby_dist1 := distance[3 * bxby_dist + bxby_distmod3];
//        if bxby_dist1 >= togo then
//          continue;
//        { TODO : Reihenfolge der Abfragen optimieren }

//        fxymoves[mvIdx] := mv;
//        Inc(mvIdx);


//        SearchUDCenter(bx1, cx1, by1, cy1, bxby_dist1, bxcx_dist1, bycy_dist1, togo - 1);

//        if found then
//          // kehre zurück, ohne mvIdx zu verändern
//          Exit;
//        Dec(mvIdx);
//      end;
//    end;

//  end;

//end;




end.
