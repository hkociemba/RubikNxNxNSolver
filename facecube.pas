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
    tmoves: array of UInt16;
    cubiCorn: array [URF .. DRB] of OrientedCorner;
    cubiEdge: array [UR .. BR] of OrientedEdge;

    mvIdx: integer; // Helper variables for search function
    found: boolean;
    fxymoves: array [0 .. 30] of moves; // f-moves und xy-slice moves
    clustersave: array [1 .. 2, 0 .. 23] of ColorIndex;
    // permutations of the remapped edgecluster. Wird mit getEdgeCluster(y)gefüllt.
    edgeCluster: array [0 .. 23] of integer;

    //procedure setSize(s: Integer);
    function getSize: integer;
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

    function MakeUDPlusCross1(x, maxMoves: integer): boolean; // Findet Zugfolge
    procedure SearchUDPlusCross1(cc, togo: integer);

    function MakeUDXCross(x, maxMoves: integer): boolean;
    procedure SearchUDXCross(ccx, slx, togo: integer);

    // phase 2
    function nextMovePh2(idx: Integer; currMove: moves): moves;
    function Phase2CenterCoord(x, y: Integer): Integer; // 0<=cc<12870
    procedure InvPhase2CenterCoord(cc, x, y: Integer);

    function Phase2SliceCoord(x, y: Integer): Integer; // 0<=cc<16
    procedure InvPhase2SliceCoord(cc, x, y: Integer);

    function MakeFBPlusCross(x, maxMoves: Integer): Boolean; // Findet Zugfolge
    procedure SearchFBPlusCross(cc, togo: Integer);
    function MakeFBFullCenter(x, y, maxMoves: Integer): Boolean;
    procedure SearchFBFullCenter(ccx, slx, ccy, togo: Integer);
    function MakeFBXCross(x, maxMoves: Integer): Boolean;
    procedure SearchFBXCross(ccx, sly, togo: Integer);

    //unused

    //function getUDBrickXYDepth(x, y: integer): integer;
    //function getUDBrickXCentXDepth(x, y: integer): integer;
    //function SearchUDCent(x, y, maxMoves: integer): boolean;
    //procedure SearchUDCenter(bx, cx, by, cy, bxby_dist, bxcx_dist,
    //  bycy_dist, togo: integer);
    //function Phase1BrickCoord(x :integer): integer; // 0<=cc<735471
    //procedure InvPhase1BrickCoord(cc, x: integer);





    procedure printMoves(i, j: integer);
    procedure applyMoves(i, j: integer);

    function clusterColorIndex(x, y, i: integer): ColorIndex;
    procedure setClusterColorIndex(x, y, i: integer; col: ColorIndex);
    //procedure getEdgeCluster(y: Integer);

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

uses Windows, main, Forms, phase1_tables,phase2_tables;
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

    S_URF3: //Not used
      ;
    S_F2:
    begin

      tmp := faceCols[Ord(U), x, y];
      faceCols[Ord(U), x, y] := faceCols[Ord(D), size - 1 - x, size - 1 - y];
      faceCols[Ord(D), size - 1 - x, size - 1 - y] := tmp;

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

      tmp := faceCols[Ord(R), x, y];
      faceCols[Ord(R), x, y] := faceCols[Ord(L), size - 1 - x, size - 1 - y];
      faceCols[Ord(L), size - 1 - x, size - 1 - y] := tmp;

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
    S_LR2:  //Only valid for clusters with reflectional symmetry!
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
  xf:=0;
  yf:=0;//initialize to prevent compiler warnings
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
  xf:=0;
  yf:=0;//initialize to prevent compiler warnings
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
//
//
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

function faceletCube.MakeUDXCross(x, maxMoves: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;

  while found = False do
  begin
    if togo > maxMoves then
      Exit(False);
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchUDXCross(Phase1CenterCoord(x, x), Phase1Brick256Coord(x, x) and $F, togo);
    Inc(togo);
  end;
  Result := True;
end;

function faceletCube.MakeUDPlusCross1(x, maxMoves: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  while found = False do
  begin
    if togo > maxMoves then
      Exit(False);
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
        if mv<xU1 then //
           SearchUDXCross(UDXCrossMove[ccx, Ord(mv)], slx, togo - 1)
        else
           SearchUDXCross(UDXCrossMove[ccx, Ord(mv)], UDBrick256Move[slx,Ord(mv)+18], togo - 1);
        if found then
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;

end;

function faceletCube.nextMovePh2(idx: Integer; currMove: moves): moves;
var
  pm: moves;
begin
  if currMove = yB3 then // done
    Exit(NoMove);
  if idx = 0 then
  begin
    if currMove = InitMove then
    begin
      Exit(fR2) // U,D moves useless
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
    while true do
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
function faceletCube.Phase2CenterCoord(x, y: Integer): Integer;
var
  occupied: array [0 .. 15] of Boolean;
  c: ColorIndex;
  i, s, k, n: Integer;
begin
  for i := 0 to 15 do
  begin
    c := clusterColorIndex(x, y, i + 8); // +8 because we start with R-face
    if (c = FCol) or (c = BCol) then
      occupied[i] := true
    else
      occupied[i] := false;
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
  result := s; // ID is 0
end;

procedure faceletCube.InvPhase2CenterCoord(cc, x, y: Integer);
var
  n, k, v, c: Integer;
  occupied: array [0 .. 15] of Boolean;
  col, setCol: ColorIndex;
begin
  for n := 0 to 15 do
    occupied[n] := false;
  n := 15;
  k := 7;
  while k >= 0 do
  begin
    v := Cnk(n, k);
    if cc < v then
    begin
      Dec(k);
      occupied[n] := true;
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
          (occupied[k] and (k < c) (* do not change already set *) ) do
          Inc(k);
        setClusterColorIndex(x, y, c + 8, setCol);
        setClusterColorIndex(x, y, k + 8, col);
      end;
      Inc(n);
      if n = 4 then // use DBCol now
        setCol := BCol;
    end;
end;


function faceletCube.MakeFBPlusCross(x, maxMoves: Integer): Boolean;
var
  idx, togo: Integer;
begin
  togo := 0;
  found := false;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  while found = false do
  begin
    if togo > maxMoves then
      Exit(false);
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchFBPlusCross(Phase2CenterCoord(x, size div 2), togo);
    Inc(togo);
  end;
  result := true;
end;

procedure faceletCube.SearchFBPlusCross(cc, togo: Integer);
var
  mv: moves;
  newcc: Integer;
begin
  if (FBPlusCrossPrun[cc] > togo) then
    Exit;
  if togo = 0 then
  begin
    found := true;
  end
  else
  begin
    mv := InitMove;
    while true do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase2Arr[NoMove, mv]
      else
        mv := nextMovePhase2Arr[fxymoves[mvIdx - 1], mv];

      if mv = NoMove then
      begin
        Exit
      end
      else
      begin

        if mv <yU1 then
            newcc := FBCenterMove[cc, Ord(mv)]
        else
          continue;//no y-moves for +cluster

        if (newcc = cc) then
          continue;

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchFBPlusCross(newcc, togo - 1);

        if found then
          // return without changing mvIdx
          Exit;
        Dec(mvIdx)
      end;
    end;

  end;
end;

// x<>y und +cross muss schon gesetzt sein!
function faceletCube.Phase2SliceCoord(x, y: Integer): Integer;
var
  m, idx: Integer;
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
  result := idx
end;


procedure faceletCube.InvPhase2SliceCoord(cc, x, y: Integer);
var
  sc, m: Integer;
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

procedure faceletCube.SearchFBFullCenter(ccx, slx, ccy, togo: Integer);
var
  mv: moves;
  sc1: SymCoord32;
  syms: UInt8;
  n, altccy, altslx, key, foundIdx: Integer;

  newccx, newccy, newslx: Integer;

begin

  Application.ProcessMessages;

  if (FBFullCenterSlicePrun[UInt64(B_16_8) * (UInt64(B_16_8) * slx + ccx) + ccy]
    > togo) then
    Exit;
  if togo = 0 then
  begin
    found := true;
  end
  else
  begin
    mv := InitMove;

    while true do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase2Arr[NoMove, mv]
      else
        mv := nextMovePhase2Arr[fxymoves[mvIdx - 1], mv];
      if (mv < xU1) and not FBfaceMoveAllowed[slx, Ord(mv)] then
        continue;

      if mv = NoMove then
      begin
        Exit
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
            newccy := FBCenterMove[ccy, Ord(mv)+18];
            newslx := FBSliceMove[slx, Ord(mv)];
          end;
          yU1..yB3:
          begin
            newccx := FBCenterMove[ccx, Ord(mv)];
            newccy := FBCenterMove[ccy, Ord(mv)-18];
            newslx := FBSliceMove[slx, Ord(mv)];
          end;
        end;



        // dies bringt 2-3s
        if (newccx = ccx) and (newslx = slx) and (newccy = ccy) then
          continue;

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchFBFullCenter(newccx, newslx, newccy, togo - 1);

        if found then
          // kehre zurück, ohne mvIdx zu verändern
          Exit;
        Dec(mvIdx)
      end;
    end;

  end;
end;



function faceletCube.MakeFBFullCenter(x, y, maxMoves: Integer): Boolean;
var
  idx, togo: Integer;
  //i: Integer;
begin
  togo := 0;
  found := false;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;

  // for i := 0 to 30 do
  // testCount[i] := 0;

  while found = false do
  begin
    if togo > maxMoves then
      Exit(false);
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchFBFullCenter(Phase2CenterCoord(x, y), Phase2SliceCoord(x, y),
      Phase2CenterCoord(y, x), togo);
    Inc(togo);
  end;
  result := true;
end;

procedure faceletCube.SearchFBXCross(ccx, sly, togo: Integer);
var
  mv: moves;
  // fAllowed: Boolean;
  sc1: SymCoord32;
  syms: UInt8;
  n, altccy, altslx, key, foundIdx: Integer;

  newccx, newccy, newslx, newsly: Integer;

begin

  Application.ProcessMessages;
  if FBXCrossPrun[B_16_8 * sly + ccx] > togo then
    Exit;
  if togo = 0 then
  begin
    found := true;
  end
  else
  begin
    mv := InitMove;
    while true do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase2Arr[NoMove, mv]
      else
        mv := nextMovePhase2Arr[fxymoves[mvIdx - 1], mv];
      if (mv < xU1) and not FBfaceMoveAllowed[sly, Ord(mv)] then
        continue;
      if mv > xB3 then
      begin
        Exit
      end
      else
      begin

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        if mv < xU1 then
          SearchFBXCross(FBXCrossMove[ccx, Ord(mv)], sly, togo - 1)
        else
          SearchFBXCross(FBXCrossMove[ccx, Ord(mv)],
            FBSliceMove[sly, Ord(mv)+18], togo - 1);
        // y-Koordinate hat die beiden low bits
        if found then
          // kehre zurück, ohne mvIdx zu verändern
          Exit;
        Dec(mvIdx)
      end;
    end;

  end;

end;

function faceletCube.MakeFBXCross(x, maxMoves: Integer): Boolean;
var
  idx, togo: Integer;
  i: Integer;
begin
  togo := 0;
  found := false;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;

  while found = false do
  begin
    if togo > maxMoves then
      Exit(false);
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchFBXCross(Phase2CenterCoord(x, x), Phase2SliceCoord(x, x) and
      $3, togo);
    // $3 um die y-slice coordinate zu erhalten, die die hinteren Bits enthält
    Inc(togo);
  end;
  result := true;
end;



//function faceletCube.SearchUDCent(x, y, maxMoves: integer): boolean;
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
//    if togo > maxMoves then
//      Exit(False);
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
