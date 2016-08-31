unit Game;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, MoveList, Board, Position;

type

  { TPly }

  TPly = class
  private
    FCommenTextInBehind: string;
    FCommentTextInFront: string;
    FMove: TMove;
    FNAG: byte;      // numeric Annotation Glyph
    FVariationLevel: word;
  public
    constructor Create(const AMove: TMove; AVariationLevel: word; ANAG: byte = 0);
    destructor Destroy; override;
  public
    property CommenTextInBehind: string read FCommenTextInBehind
      write FCommenTextInBehind;
    property CommentTextInFront: string read FCommentTextInFront
      write FCommentTextInFront;
    property ChessMove: TMove read FMove write FMove;
    property NAG: byte read FNAG write FNAG;
    property VariationLevel: word read FVariationLevel write FVariationLevel;
  end;

  TPlyList = specialize TFPGObjectList<TPly>;

  { TGame }

  TGame = class
  private
    FBoard: TBoard;
    FCurrentPlyNumber: word;
    FPlyList: TPlyList;
    StartPosition: TPosition;
  public
    procedure AddMove(AMove: TMove);
    constructor Create(ABoard: TBoard); virtual;
    procedure Clear;
    destructor Destroy; override;
    // Setups position before last move on board
    procedure GoOneMoveBackward;
    // Setups position after next move on board
    procedure GoOneMoveForward;
    // Setups position after the given ply number (0 is then the initial position)
    procedure GoToPositionAfterPlyNumber(Index: word);
    property CurrentPlyNumber: word read FCurrentPlyNumber write FCurrentPlyNumber;
  end;

  { TStandardGame }

  TStandardGame = class(TGame)
  public
    constructor Create(ABoard: TBoard); override;
  end;

implementation

{ TPly }

constructor TPly.Create(const AMove: TMove; AVariationLevel: word; ANAG: byte);
begin
  FMove := AMove;
  FVariationLevel := AVariationLevel;
  FNAG := ANAG;
end;

destructor TPly.Destroy;
begin
  FMove.Free;
  inherited Destroy;
end;

{ TGame }

procedure TGame.AddMove(AMove: TMove);
begin
  if FPlyList.Count > 0 then
  begin
    FPlyList.Add(TPly.Create(AMove, FPlyList.Last.VariationLevel));
  end
  else
  begin
    StartPosition.Copy(FBoard.CurrentPosition);
    FPlyList.Add(TPly.Create(AMove, 0));
  end;
  Inc(FCurrentPlyNumber);
  FBoard.CurrentPosition.PlayMove(AMove);
end;

constructor TGame.Create(ABoard: TBoard);
begin
  FCurrentPlyNumber := 0;
  FBoard := ABoard;
  StartPosition := TStandardPosition.Create;
  FPlyList := TPlyList.Create(True);
end;

procedure TGame.Clear;
begin
  FCurrentPlyNumber := 0;
  FPlyList.Clear;
end;

destructor TGame.Destroy;
begin
  FPlyList.Free;
  StartPosition.Free;
  inherited Destroy;
end;

procedure TGame.GoOneMoveBackward;
begin
  GoToPositionAfterPlyNumber(FCurrentPlyNumber - 1);
end;

procedure TGame.GoOneMoveForward;
begin
  GoToPositionAfterPlyNumber(FCurrentPlyNumber + 1);
end;

procedure TGame.GoToPositionAfterPlyNumber(Index: word);
var
  i: integer;
begin
  FBoard.CurrentPosition.Copy(StartPosition);
  for i := 0 to Index - 1 do
  begin
    // TODO: Add support for variations
    FBoard.CurrentPosition.PlayMove(FPlyList.Items[i].ChessMove);
  end;
  FCurrentPlyNumber := Index;
  FBoard.Invalidate;
end;

{ TStandardGame }

constructor TStandardGame.Create(ABoard: TBoard);
begin
  inherited Create(ABoard);
  FBoard.CurrentPosition := TStandardPosition.Create;
end;

end.
