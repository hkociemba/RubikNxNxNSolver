unit cubedefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

const
  UDOPTIMAL = True; // langsamere aber optimale Version wenn true

type

  ColorIndex = (UCol = 0, DCol, RCol, LCol, FCol, BCol, NoCol);
  Axis = (U = 0, D, R, L, F, B);
  // f-moves sind facemoves,x-moves und y-moves sind moves der x-y-orbit slices
  Moves = (InitMove = -1, fU1, fU2, fU3, fD1, fD2, fD3, fR1, fR2, fR3, fL1, fL2,
    fL3, fF1, fF2, fF3, fB1, fB2, fB3, xU1, xU2, xU3, xD1, xD2, xD3, xR1, xR2,
    xR3, xL1, xL2, xL3, xF1, xF2, xF3, xB1, xB2, xB3, yU1, yU2, yU3, yD1, yD2,
    yD3, yR1, yR2, yR3, yL1, yL2, yL3, yF1, yF2, yF3, yB1, yB2, yB3, NoMove);

  Symmetry = (S_URF3, S_F2, S_U4, S_LR2, S_R4, S_F4); // Grundsymmetrien

  Corner = (URF = 0, UFL, ULB, UBR, DFR, DLF, DBL, DRB, NNN);
  Edge = (UR, UF, UL, UB, DR, DF, DL, DB, FR, FL, BL, BR, NN);

  CornerStatus = (CORNNOERROR, CORNPERMUTATIONERROR, CORNORIENTATIONERROR);
  EdgeStatus = (EDGENOERROR, EDGEPERMUTATIONERROR, EDGEORIENTATIONERROR,
    EDGESIZEERROR);

  OrientedCorner = record
    c: Corner;
    o: shortint;
  end;

  OrientedEdge = record
    e: Edge;
    o: shortint;
  end;

  SymCoord32 = record
    c_idx: UInt32;
    sym: UInt16;
  end;

  SymCoord16 = record
    c_idx: UInt16;
    sym: UInt16;
  end;

  States32 = record
    state: array of UInt32;
    used: integer;
  end;

  CornerColorIndex = array [URF .. DRB, 0 .. 2] of ColorIndex;
  EdgeColorIndex = array [UR .. BR, 0 .. 1] of ColorIndex;
  EdgeClusterColorIndex = array [0 .. 23, 0 .. 1] of ColorIndex;
  CFPosition = array [URF .. DRB, 0 .. 2] of integer;
  EFPosition = array [UR .. BR, 0 .. 1] of integer;
  ECFPosition = array [0 .. 23, 0 .. 1] of integer;

var
  Color: array [UCol .. NoCol] of TColor; // Global colors of Cube

const
  UDCenterID = 0; // Koordinate des Zielzustandes der UD-Center in Phase1

  B_24_8 = 735471; // Binomial(24,8)
  N_CHUNK = 45967; //Binomial(24,8) div 16 + 1
  B_16_8 = 12870; // Binomial(16,8)
  B_8_4 = 70; // Binomial(8,4)
  B_24_12 = 2704156;
  N_SYMCENTCOORD = 92247; // Anzahl der Äquivalenzklassen der UDCenterCoord
  N_SYMBRICKCOORD = 46935; // Anzahl der Äquivalenzklassen der UDBrickCoord
  N_SYMBRICK702COORD = 690; //Number of equivalence classes of Ph3Brick702Coord

  invMove: array[InitMove..Nomove] of Moves =( initMove, fU3, fU2, fU1, fD3, fD2, fD1, fR3, fR2, fR1, fL3, fL2,
    fL1, fF3, fF2, fF1, fB3, fB2, fB1, xU3, xU2, xU1, xD3, xD2, xD1, xR3, xR2,
    xR1, xL3, xL2, xL1, xF3, xF2, xF1, xB3, xB2, xB1, yU3, yU2, yU1, yD3, yD2,
    yD1, yR3, yR2, yR1, yL3, yL2, yL1, yF3, yF2, yF1, yB3, yB2, yB1, NoMove);


  // wohin facelets bei  180 Drehungen, deren slice durch U,R,F gehen, abgebildet werden.
  oppositeU: array [0 .. 23] of integer = (-1, -1, -1, -1, -1, -1, -1, -1, 14,
    15, 12, 13, -1, -1, -1, -1, 22, 23, 20, 21, -1, -1, -1, -1);
  oppositeR: array [0 .. 23] of integer =
    (6, 7, 4, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 20,
    21, 22, 23, -1, -1, -1, -1);
  oppositeF: array [0 .. 23] of integer = (4, 5, 6, 7, -1, -1, -1, -1, 12, 13,
    14, 15, -1, -1, -1, -1, -1, -1, -1, 1, -1, -1, -1, -1);

  // wohin facelets bei  180 Drehungen, deren slice durch U,R,F gehen, abgebildet werden.
  opposite: array [0 .. 2, 0 .. 23] of integer =
    ((-1, -1, -1, -1, -1, -1, -1, -1, 14, 15, 12, 13, -1, -1, -1, -1,
    22, 23, 20, 21, -1, -1, -1, -1),
    (6, 7, 4, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 20,
    21, 22, 23, -1, -1, -1, -1), (4, 5, 6, 7, -1, -1, -1, -1, 12, 13,
    14, 15, -1, -1, -1, -1, -1, -1, -1, 1, -1, -1, -1, -1));

  Phase2Allowed: array [-1 .. 54] of boolean = (True { initMove }, False,
    False, False, False, False, False, { fU1..fD3 }
    True, True, True, True, True, True, { fR1..fL3 }
    True, True, True, True, True, True, { fF1..fB3 }
    True, True, True, True, True, True, { xU1..xD3 }
    False, True, False, False, True, False, { xR1..xL3 }
    False, True, False, False, True, False, { xF1..xB3 }
    True, True, True, True, True, True, { yU1..yD1 }
    False, True, False, False, True, False, { yR1..yL3 }
    False, True, False, False, True, False, { yF1..yB3 }
    True { noMove }
    );

  { TODO : fU1..fD3 make false }

  // In phase 3, square moves of slices or any face turns are allowed
  Phase3Allowed: array [-1 .. 54] of boolean = (True { initMove }, True,
    True, True, True, True, True, { fU1..fD3 }
    True, True, True, True, True, True, { fR1..fL3 }
    True, True, True, True, True, True, { fF1..fB3 }
    False, True, False, False, True, False, { xU1..xD1 }
    False, True, False, False, True, False, { xR1..xL3 }
    False, True, False, False, True, False, { xF1..xB3 }
    False, True, False, False, True, False, { yU1..yD1 }
    False, True, False, False, True, False, { yR1..yL3 }
    False, True, False, False, True, False, { yF1..yB3 }
    True { noMove }
    );


  // In phase 4, only U, D face moves and square moves of the orthogonal slices
  Phase4Allowed: array [-1 .. 54] of boolean = (True { initMove }, True,
    True, True, True, True, True, { fU1..fD3 }
    False, False, False, False, False, False, { fR1..fL3 }
    False, False, False, False, False, False, { fF1..fB3 }
    False, False, False, False, False, False, { xU1..xD1 }
    False, True, False, False, True, False, { xR1..xL3 }
    False, True, False, False, True, False, { xF1..xB3 }
    False, False, False, False, False, False, { yU1..yD1 }
    False, True, False, False, True, False, { yR1..yL3 }
    False, True, False, False, True, False, { yF1..yB3 }
    True { noMove }
    );

  // +++++++++++++the colors of the corner cubies++++++++++++++++++++++++++++++++++
  CCI: CornerColorIndex = ((UCol, RCol, FCol), (UCol, FCol, LCol),
    (UCol, LCol, BCol), (UCol, BCol, RCol), (DCol, FCol, RCol),
    (DCol, LCol, FCol), (DCol, BCol, LCol), (DCol, RCol, BCol));

  // +++++++++++++++++++++++the colors of the middle edge cubies++++++++++++++++++++++++++
  ECI: EdgeColorIndex = ((UCol, RCol), (UCol, FCol), (UCol, LCol), (UCol, BCol),
    (DCol, RCol), (DCol, FCol), (DCol, LCol), (DCol, BCol), (FCol, RCol),
    (FCol, LCol), (BCol, LCol), (BCol, RCol));

  // +++++++++++++++++++++++the colors of the 24 C(0,y) cluster cubies++++++++++++++++++
  ECCI: EdgeClusterColorIndex = ((UCol, BCol), (UCol, RCol), (UCol, FCol),
    (UCol, LCol), (DCol, FCol), (DCol, RCol), (DCol, BCol), (DCol, LCol),
    (RCol, UCol), (RCol, BCol), (RCol, DCol), (RCol, FCol), (LCol, UCol),
    (LCol, FCol), (LCol, DCol), (LCol, BCol), (FCol, UCol), (FCol, RCol),
    (FCol, DCol), (FCol, LCol), (BCol, UCol), (BCol, LCol), (BCol, DCol),
    (BCol, RCol));

  // ++++the involved facelets of the corner cubies+++++++++++++++++++++++
  // 0 means facelet is in row 0, 1 means it is in row size-1
  CFRow: CFPosition = ((1, 0, 0), (1, 0, 0), (0, 0, 0), (0, 0, 0), (0, 1, 1),
    (0, 1, 1), (1, 1, 1), (1, 1, 1));
  // the same for the columns
  CFCol: CFPosition = ((1, 0, 1), (0, 0, 1), (0, 0, 1), (1, 0, 1), (1, 1, 0),
    (0, 1, 0), (0, 1, 0), (1, 1, 0));

  // ++++the involved facelets of the middle edge cubies+++++++++++++++++++++++
  // size is supposed to be odd
  // 0 means row/col 0, 1 means row/col (size-1)/2, 2 means row/col size-1
  EFRow: EFPosition = ((1, 0), (2, 0), (1, 0), (0, 0), (1, 2), (0, 2), (1, 2),
    (2, 2), (1, 1), (1, 1), (1, 1), (1, 1));
  EFCol: EFPosition = ((2, 1), (1, 1), (0, 1), (1, 1), (2, 1), (1, 1), (0, 1),
    (1, 1), (2, 0), (0, 2), (2, 0), (0, 2));

  // ++++the involved facelets of the edge C(0,y) cluster cubies+++++++++++++++++++++++
  // 0 means row/col 0, 1 means row/col y, 2 means row/col size-1,
  // 3 means row/col size-1-y
  ECFRow: ECFPosition = ((0, 0), (1, 0), (2, 0), (3, 0), (0, 2), (1, 2), (2, 2),
    (3, 2), (0, 3), (1, 1), (2, 3), (3, 3), (0, 1), (1, 1), (2, 1), (3, 3),
    (0, 2), (1, 1), (2, 0), (3, 3), (0, 0), (1, 1), (2, 2), (3, 3));
  ECFCol: ECFPosition = ((1, 3), (2, 3), (3, 3), (0, 3), (1, 1), (2, 1), (3, 1),
    (0, 1), (1, 2), (2, 0), (3, 2), (0, 2), (1, 0), (2, 0), (3, 0), (0, 2),
    (1, 1), (2, 0), (3, 3), (0, 2), (1, 3), (2, 0), (3, 1), (0, 2));

  // for EdgeCluster C(0,y) change indexing such that 0..11 denote
  // the elements under the orbit of yU2,yD2,yR2,yL2,yF2,YB2,U,D,R,L,F2,B2
  remapEdges: array [0 .. 23] of integer = (0, 1, 2, 3, 4, 5, 6, 7, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 8, 21, 9, 22, 10, 23, 11);
  invRemapEdges: array [0 .. 23] of integer = (0, 1, 2, 3, 4, 5, 6, 7, 17, 19,
    21, 23, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 22);
  // Refers to type Moves above
  MoveStrings: array [InitMove .. NoMove] of string = ('InitMove', 'fU1', 'fU2',
    'fU3', 'fD1', 'fD2', 'fD3', 'fR1', 'fR2', 'fR3', 'fL1', 'fL2', 'fL3', 'fF1',
    'fF2', 'fF3', 'fB1', 'fB2', 'fB3', 'xU1', 'xU2', 'xU3', 'xD1', 'xD2', 'xD3',
    'xR1', 'xR2', 'xR3', 'xL1', 'xL2', 'xL3', 'xF1', 'xF2', 'xF3', 'xB1', 'xB2',
    'xB3', 'yU1', 'yU2', 'yU3', 'yD1', 'yD2', 'yD3', 'yR1', 'yR2', 'yR3', 'yL1',
    'yL2', 'yL3', 'yF1', 'yF2', 'yF3', 'yB1', 'yB2', 'yB3', 'NoMove');
  ColorIndexStrings: array [UCol .. NoCol] of string = ('U', 'D', 'R', 'L', 'F',
    'B', '-');

  //edgecluster  indeices of edges
  edgeidx: EFPosition = ((1, 12), (2, 20), (3, 16), (0, 22), (5, 14), (4, 21), (7, 18),
    (6, 23), (8, 15), (9, 17), (10, 19), (11, 13));

implementation

end.
