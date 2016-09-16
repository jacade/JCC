unit PGNGame;

{$mode objfpc}{$H+}

//{$DEFINE LOGGING}

interface

uses
  Classes, SysUtils, FileUtil, RegExpr, Game, MoveList, Position, Pieces
  {$IFDEF Logging}
  , Ply
  {$ENDIF}  ;
// as in http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm

const
  NO_RATING = -1; // specifies that a player has no ELO, USCF, etc.

type
  EPGNImportException = Exception;

  TPGNTag = record
    Name: string;
    Value: string;
  end;

  { TPGNGame }

  TPGNGame = class(TStandardGame)
  private
    FAnnotator: string;
    FBlackELO: integer;
    FBlackNA: string;
    FBlackTitle: TFIDETitle;
    FBlackType: TPlayerType;
    FBlackUSCF: integer;
    FBoard: integer;
    FECO: string;
    FEventDate: string;
    FEventSponsor: string;
    FFen: string;
    FEvent, FSite, FDate, FRound, FWhite, FBlack, FResult: string;
    FMode: TModeType;
    FNIC: string;
    FOpening: string;
    FPlyCount: word;
    FSection: string;
    FSetUp: boolean;
    FStage: string;
    FSubVariation: string;
    FTermination: TTerminationType;
    FTime: string;
    FTimeControl: string;
    FUTCDate: string;
    FUTCTime: string;
    FVariation: string;
    FWhiteELO: integer;
    FWhiteNA: string;
    FWhiteTitle: TFIDETitle;
    FWhiteType: TPlayerType;
    FWhiteUSCF: integer;
    function PGNToMove(const APGNMove: string): TMove;
  public
    // These procedures add a move in pgn format e.g. Nf3, e8=Q
    procedure AddPGNMove(const APGNMove: string);
    procedure AddPGNMoveAsSideLine(const APGNMove: string);
  public
    // Seven Tag Roster
    property Event: string read FEvent write FEvent;
    property Site: string read FSite write FSite;
    property Date: string read FDate write FDate;
    property Round: string read FRound write FRound;
    property White: string read FWhite write FWhite;
    property Black: string read FBlack write FBlack;
    property Result: string read FResult write FResult;
  public   // Player related information  TODO: Add more national ranking systems like DWZ
    property BlackELO: integer read FBlackELO write FBlackELO;
    property BlackNA: string read FBlackNA write FBlackNA;
    property BlackTitle: TFIDETitle read FBlackTitle write FBlackTitle;
    property BlackType: TPlayerType read FBlackType write FBlackType;
    property BlackUSCF: integer read FBlackUSCF write FBlackUSCF;
    property WhiteELO: integer read FWhiteELO write FWhiteELO;
    property WhiteNA: string read FWhiteNA write FWhiteNA;
    property WhiteTitle: TFIDETitle read FWhiteTitle write FWhiteTitle;
    property WhiteType: TPlayerType read FWhiteType write FWhiteType;
    property WhiteUSCF: integer read FWhiteUSCF write FWhiteUSCF;
  public  // Event related information
    property Board: integer read FBoard write FBoard;
    property EventDate: string read FEventDate write FEventDate;
    property EventSponsor: string read FEventSponsor write FEventSponsor;
    property Section: string read FSection write FSection;
    property Stage: string read FStage write FStage;
  public  // Opening information (locale)
    property Opening: string read FOpening write FOpening;
    property SubVariation: string read FSubVariation write FSubVariation;
    property Variation: string read FVariation write FVariation;
  public  // Opening information (3rd party)
    property ECO: string read FECO write FECO;
    property NIC: string read FNIC write FNIC;
  public  // Time and date information
    property Time: string read FTime write FTime;
    property UTCDate: string read FUTCDate write FUTCDate;
    property UTCTime: string read FUTCTime write FUTCTime;
  public  // Time control
    // TODO: Replace string with own format, when chessclock is ready
    property TimeControl: string read FTimeControl write FTimeControl;
  public  // Alternative starting position
    property FEN: string read FFen write FFEN;
    property SetUp: boolean read FSetUp write FSetUp;
  public  // Game conclusion
    property Termination: TTerminationType read FTermination write FTermination;
  public  // Miscellanous
    property Annotator: string read FAnnotator write FAnnotator;
    property Mode: TModeType read FMode write FMode;
    property PlyCount: word read FPlyCount write FPlyCount;
  end;

implementation


{ TPGNGame }

function TPGNGame.PGNToMove(const APGNMove: string): TMove;
var
  Move: TMove;
  Piece, PromoPiece: TPieceType;
  AFile, ARank: byte;
  Start: TSquare10x12;
  s: string;
  Dest: TAlgebraicSquare;
  Temp: TMoveList;
  NotValid: boolean;
begin
  // NOTE: This is very strict and only works with pgn export format
  // However this should be sufficient in most of the cases
  s := APGNMove;
  // We don't need signs for check, mate or a capture, so we delete them
  if (s[Length(s)] = '+') or (s[Length(s)] = '#') then
    Delete(s, Length(s), 1);
  if Pos('x', s) > 0 then
    Delete(s, Pos('x', s), 1);
  // Now s should be O-O, O-O-O or like [N, B, R, Q, K]?[a-h]?[1-8]?[a-h][1-8](=[N, B, R, Q, K])?
  if s = 'O-O' then
  begin
    if FCurrentPosition.WhitesTurn then
      Result := CreateMove(95, 97)
    else
      Result := CreateMove(25, 27);
  end
  else
  if s = 'O-O-O' then
  begin
    if FCurrentPosition.WhitesTurn then
      Result := CreateMove(95, 93)
    else
      Result := CreateMove(25, 23);
  end
  else
  begin
    // Get the piece and delete it afterwards from s
    case s[1] of
      'N': Piece := PieceType(bptKnight, FCurrentPosition.WhitesTurn);
      'B': Piece := PieceType(bptBishop, FCurrentPosition.WhitesTurn);
      'R': Piece := PieceType(bptRook, FCurrentPosition.WhitesTurn);
      'Q': Piece := PieceType(bptQueen, FCurrentPosition.WhitesTurn);
      'K': Piece := PieceType(bptKing, FCurrentPosition.WhitesTurn);
      'a'..'h', 'P': Piece := PieceType(bptPawn, FCurrentPosition.WhitesTurn);
    end;
    if s[1] in ['B', 'K', 'N', 'P', 'Q', 'R'] then
      Delete(s, 1, 1);
    // Look whether the move is a promotion
    if s[Length(s) - 1] = '=' then
    begin
      case s[Length(s)] of
        'N': PromoPiece := PieceType(bptKnight, FCurrentPosition.WhitesTurn);
        'B': PromoPiece := PieceType(bptBishop, FCurrentPosition.WhitesTurn);
        'R': PromoPiece := PieceType(bptRook, FCurrentPosition.WhitesTurn);
        'Q': PromoPiece := PieceType(bptQueen, FCurrentPosition.WhitesTurn);
      end;
      // Now we know the promotion piece and so we delete the last two chars
      Delete(s, Length(s) - 1, 2);
    end
    else
      PromoPiece := ptEmpty;
    // Get the Destination square and delete it from s afterwards
    Dest := AlgebraicSquare(s[Length(s) - 1], s[Length(s)]);
    Delete(s, Length(s) - 1, 2);
    // There is still some information left
    AFile := 0;
    Arank := 0;
    Start := 0;
    case Length(s) of
      0: ;// Noting
      1:
      begin  // There is either a file or a rank left
        if s[1] in ['a'..'h'] then
        begin
          AFile := Ord(s[1]) - 96;
        end
        else
        if s[1] in ['1'..'8'] then
        begin
          ARank := Ord(s[1]) - 48;
        end;
      end;
      2: Start := AlgebraicSquare(s[1], s[2]);
      else
        raise Exception.Create(APGNMove + ' is longer than expected');
    end;
    // Get all moves which match the extracted information
    Temp := (FCurrentPosition as TStandardPosition).FilterLegalMoves(Piece,
      Start, Dest, PromoPiece);
    case Temp.Count of
      0: raise Exception.Create(APGNMove + ' is no valid move.');
      1: Result := temp.Items[0];
      else // we need to extract more information from s
        for Move in Temp do
        begin
          if ((AFile > 0) and (Move.Start.RFile = AFile)) or
            ((ARank > 0) and (Move.Start.RRank = ARank)) then
            Result := Move;
        end;
    end;
    Temp.Free;
  end;
  //if Result = nil then
  //  raise Exception.Create(APGNMove + ' is no valid move.');
end;

{$IFDEF Logging}
procedure PrintTree(const AData: TPly);
begin
  if AData <> nil then
    Write(SquareToString(AData.Move.Start), SquareToString(AData.Move.Dest), '|');
end;

{$ENDIF}

procedure TPGNGame.AddPGNMove(const APGNMove: string);
var
  temp: TMove;
begin
  temp := PGNToMove(APGNMove);
  {$IFDEF Logging}
  WriteLn(SquareToString(temp.Start), SquareToString(temp.Dest));
  {$ENDIF}
  AddMove(temp);
  {$IFDEF Logging}
  FPlyTree.DepthFirstTraverse(@PrintTree);
  WriteLn;
  {$ENDIF}
end;

procedure TPGNGame.AddPGNMoveAsSideLine(const APGNMove: string);
begin
  AddMoveAsSideLine(PGNToMove(APGNMove));
{$IFDEF Logging}
  FPlyTree.DepthFirstTraverse(@PrintTree);
  WriteLn;
{$ENDIF}
end;

end.
