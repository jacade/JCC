{ JCC (Jan's Chess Componenents) - This file implements the Universal Chess Interface
  Copyright (C) 2016-2017  Jan Dette

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit UCI;

// Based on http://www.shredderchess.de/schach-info/features/uci-universal-chess-interface.html

{$mode objfpc}{$H+}

//{$DEFINE Logging}

interface

uses
  Classes, SysUtils, ExtCtrls, process, RegExpr, UCIOptions, StrTools, MoveList, Pieces;

type

  TScore = record
    CP: integer;            // the score in centipawns
    Mate: integer;          // mate in y moves
    LowerBound: boolean;    // the score is just a lower bound
    UpperBound: boolean;    // the score is just an upper bound
  end;

  TInfo = record
    Depth: integer;          // search depth in plies
    SelDepth: integer;       // selective search depth in plies
    Time: integer;           // the time searched in ms
    Nodes: DWord;            // x nodes searched
    PV: TMoveList;           // the best line found
    MultiPV: integer;        // this for the multi pv mode
    Score: TScore;
    CurrMove: string;        // currently searching this move
    CurrMoveNumber: integer; // currently searching move number x
    HashFull: integer;       // the hash is x permill full
    NPS: DWord;              // x nodes per second searched
    TBHits: integer;         // x positions where found in the endgame table bases
    SBHits: integer;         // x positions where found in the shredder endgame databases
    CPULoad: integer;        // the cpu usage of the engine is x permill
    Str: string;             // any string str which will be displayed be the engine
    Refutation: TMoveList;   // move <move1> is refuted by the line <move2> ... <movei>
    CurrLine: TMoveList;     // this is the current line the engine is calculating
  end;

  TInfoMaskElement = (imDepth, imSelDepth, imTime, imNodes, imPV,
    imMultiPV, imScore, imCP, imMate, imLowerBound, imUpperBound, imCurrMove,
    imCurrMoveNumber, imHashFull, imNPS, imTBHits, imSBHits, imCPULoad,
    imStr, imRefutation, imCurrLine);
  TInfoMask = set of TInfoMaskElement;

  TStatus = (stChecking, stError, stOk);

  // These are the commands the engine could send and that have to be handled by the GUI
  TOnBestMove = procedure(Sender: TObject; BestMove, Ponder: TMove) of object;
  TOnCopyProtection = procedure(Sender: TObject; Status: TStatus) of object;
  TOnInfo = procedure(Sender: TObject; Info: TInfo; InfoMask: TInfoMask) of object;
  TOnQuit = TNotifyEvent;
  TOnReadyOk = TNotifyEvent;
  TOnRegistration = procedure(Sender: TObject; Status: TStatus) of object;

  { TUCIEngine }

  TUCIEngine = class(TComponent)
    procedure TimerTimer(Sender: TObject);
  private
    FEngine: TProcess;
    FDebug: boolean;
    FisRunning: boolean;
    FName, FAuthor: string;
    FHash: integer;
    FPocessName: string;
    Output: TStringList;
    FOnBestMove: TOnBestMove;
    FOnCopyProtection: TOnCopyProtection;
    FOnInfo: TOnInfo;
    FOptions: TUCIOptionList;
    FOnQuit: TOnQuit;
    FOnReadyOk: TOnReadyOk;
    FOnRegistration: TOnRegistration;
    Timer: TTimer;
    TOnCopyProtection: TOnCopyProtection;
    UCIOk: boolean;
    procedure GetLine;
    procedure ParseBestMove(const s: string);
    procedure ParseCopyProtection(const s: string);
    procedure ParseID(const s: string);
    procedure ParseInfo(const s: string);
    procedure ParseOption(const s: string);
    procedure ParseOutput;
    procedure ParseRegistration(const s: string);
    procedure SendStr(msg: string);
    procedure SetDebug(AValue: boolean);
    procedure SetProcessName(AValue: string);
    procedure WaitForToken(const Text: string; Interval: integer = 100);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyOption(Option: TUCIOption);
    procedure Go(searchmoves: TStringList = nil; ponder: boolean = False;
      wtime: integer = 0; btime: integer = 0; winc: integer = 0;
      binc: integer = 0; movestogo: integer = -1; depth: integer = 0;
      nodes: integer = 0; mate: integer = 0; movetime: integer = 0;
      infinite: boolean = False);
    procedure Init;
    procedure NewGame;
    procedure PonderHit;
    procedure Quit;
    procedure SendIsReady;
    procedure SendRegistration(Later: boolean; AName, code: string);
    procedure SetUpPosition(FEN: string = 'startpos'; Moves: TMoveList = nil);
    procedure Stop;
    property Author: string read FAuthor;
    property EngineName: string read FName;
    property Hash: integer read FHash;
    property isRunning: boolean read FisRunning;
    property Options: TUCIOptionList read FOptions;
  published
    property Debug: boolean read FDebug write SetDebug default False;
    property OnBestMove: TOnBestMove read FOnBestMove write FOnBestMove;
    property OnCopyProtection: TOnCopyProtection
      read FOnCopyProtection write TOnCopyProtection;
    property OnInfo: TOnInfo read FOnInfo write FOnInfo;
    property OnQuit: TOnQuit read FOnQuit write FOnQuit;
    property OnReadyOk: TOnReadyOk read FOnReadyOk write FOnReadyOk;
    property OnRegistration: TOnRegistration read FOnRegistration write FOnRegistration;
    property ProcessName: string read FPocessName write SetProcessName;
  end;

implementation

{ TUCIEngine }

procedure TUCIEngine.TimerTimer(Sender: TObject);
begin
  GetLine;
  ParseOutput;
end;

// s. http://wiki.lazarus.freepascal.org/Executing_External_Programs#Reading_large_output
procedure TUCIEngine.GetLine;
var
  Buffer: string;
  BytesAvailable: DWord;
  p: integer;
  {$IFDEF Logging}
  i, l: integer;
  {$ENDIF}
begin
  {$IFDEF Logging}
  l := Output.Count;
  {$ENDIF}
  BytesAvailable := FEngine.Output.NumBytesAvailable;

  while BytesAvailable > 0 do
  begin
    SetLength(Buffer, BytesAvailable);
    FEngine.OutPut.Read(Buffer[1], BytesAvailable);

    // Separiere Ausgabe
    while Pos(#10, buffer) > 0 do
    begin
      p := Pos(#10, Buffer);
      Output.Add(Copy(Buffer, 1, p - 1));
      Delete(Buffer, 1, p);
    end;
    Output.Add(Buffer);
    BytesAvailable := FEngine.Output.NumBytesAvailable;
  end;

  {$IFDEF Logging}
  for i := l to Output.Count - 1 do
    WriteLn('OUTPUT: ', Output.Strings[i]);
  {$ENDIF}
end;

procedure TUCIEngine.ParseBestMove(const s: string);
var
  st: string;
  bp: array[1..2] of string;
  i: byte;
  Start, Dest: TAlgebraicSquare;
  Moves: array[1..2] of TMove;
  Piece: TPieceType;
begin
  if not Assigned(FOnBestMove) then
    Exit;
  st := s;
  Delete(st, 1, Pos('bestmove', s) + 8);
  st := Trim(st);
  bp[1] := '';
  bp[2] := '';
  if Pos('ponder', st) > 0 then
  begin
    i := 1;
    while st[i] <> ' ' do
    begin
      bp[1] := bp[1] + st[i];
      Inc(i);
    end;
    Delete(st, 1, Pos('ponder', st) + 6);
    st := Trim(st);
    bp[2] := st;
  end
  else
    bp[1] := st;
  {$IFDEF Logging}
  WriteLn('Found Best Move: ', bp[1], 'Found Ponder: ', bp[2]);
  {$ENDIF}
  for i := 1 to 2 do
  begin
    if Length(bp[i]) > 3 then
    begin
      Start.RFile := bp[i][1];
      Start.RRank := bp[i][2];
      Dest.RFile := bp[i][3];
      Dest.RRank := bp[i][4];
      if Length(bp[i]) = 5 then
      begin
        if bp[i][4] = '8' then
          case bp[i][5] of
            'r': Piece := ptWRook;
            'b': Piece := ptWBishop;
            'n': Piece := ptWKnight;
            'q': Piece := ptWQueen;
          end
        else
          case bp[i][5] of
            'r': Piece := ptBRook;
            'b': Piece := ptBBishop;
            'n': Piece := ptBKnight;
            'q': Piece := ptBQueen;
          end;
        Moves[i] := CreateMove(Start, Dest, Piece);
      end
      else
        Moves[i] := CreateMove(Start, Dest);
    end
    else
      Moves[i] := nil;
  end;
  FOnBestMove(Self, Moves[1], Moves[2]);
end;

procedure TUCIEngine.ParseCopyProtection(const s: string);
var
  st: string;
  chk, err, ok: TRegExpr;
  Stat: TStatus;
begin
  if not Assigned(FOnCopyProtection) then
    Exit;
  st := s;
  chk := TRegExpr.Create;
  err := TRegExpr.Create;
  ok := TRegExpr.Create;
  chk.Expression := '[ \t]*copyprotection[ \t]*checking[ \t]*';
  err.Expression := '[ \t]*copyprotection[ \t]*error[ \t]*';
  ok.Expression := '[ \t]*copyprotection[ \t]*ok[ \t]*';
  if chk.Exec(st) then
    Stat := stChecking;
  if err.Exec(st) then
    Stat := stError;
  if ok.Exec(st) then
    Stat := stOk;
  FOnCopyProtection(Self, Stat);
  FreeAndNil(chk);
  FreeAndNil(err);
  FreeAndNil(ok);
end;

procedure TUCIEngine.ParseID(const s: string);
var
  st: string;
  nm, au: TRegExpr;
begin
  st := Trim(s);
  nm := TRegExpr.Create;
  au := TRegExpr.Create;
  nm.Expression := '[ \t]*id[ \t]*name.*';
  au.Expression := '[ \t]*id[ \t]*author.*';
  if nm.exec(st) then
  begin
    Delete(st, 1, Pos('name', st) + 4);
    FName := Trim(st);
    {$IFDEF Logging}
    WriteLn('FOUND Name: ', FName);
    {$ENDIF}
  end;
  if au.exec(st) then
  begin
    Delete(st, 1, Pos('author', st) + 6);
    FAuthor := Trim(st);
    {$IFDEF Logging}
    WriteLn('FOUND Author: ', FAuthor);
    {$ENDIF}
  end;
  FreeAndNil(nm);
  FreeAndNil(au);
end;

procedure TUCIEngine.ParseInfo(const s: string);

  function StringsToMoveList(Strings: TStringList): TMoveList;
  var
    s: string;
    Start, Dest: TAlgebraicSquare;
    Piece: TPieceType;
  begin
    Result := TMoveList.Create;
    for s in Strings do
    begin
      Start.RFile := s[1];
      Start.RRank := s[2];
      Dest.RFile := s[3];
      Dest.RRank := s[4];
      Piece := ptEmpty;
      if Length(s) = 5 then
      begin
        if s[4] = '8' then
          case s[5] of
            'r': Piece := ptWRook;
            'b': Piece := ptWBishop;
            'n': Piece := ptWKnight;
            'q': Piece := ptWQueen;
          end
        else
          case s[5] of
            'r': Piece := ptBRook;
            'b': Piece := ptBBishop;
            'n': Piece := ptBKnight;
            'q': Piece := ptBQueen;
          end;
      end;
      Result.Add(CreateMove(Start, Dest, Piece));
    end;
    Strings.Free;
  end;

const
  Tokens: array[1..21] of string =
    ('depth', 'seldepth', 'time', 'nodes', 'pv', 'multipv', 'score',
    'cp', 'mate', 'lowerbound', 'upperbound', 'currmove', 'currmovenumber',
    'hashfull', 'nps', 'tbhits', 'sbhits', 'cpuload', 'string',
    'refutation', 'currline');
var
  Info: TInfo;
  InfoMask: TInfoMask;
  temp: TStringArray;
  i: integer;
  st: string;
begin
  if not Assigned(FOnInfo) then
    Exit;
  Info.PV := nil;
  Info.CurrLine := nil;
  Info.Refutation := nil;
  st := s;
  // Delete 'info' from beginning
  Delete(st, 1, 4);
  temp := GetValuesOfKeys(st, Tokens);
  InfoMask := [];
  for i := 0 to 20 do
    if Length(temp[i]) > 0 then
    begin
      case i of
        0: Info.Depth := StrToInt(temp[0]);
        1: Info.SelDepth := StrToInt(temp[1]);
        2: Info.Time := StrToInt(temp[2]);
        3: Info.Nodes := StrToDWord(temp[3]);
        4: Info.PV := StringsToMoveList(Split(temp[4], ' '));
        5: Info.MultiPV := StrToInt(temp[5]);
        7: Info.Score.CP := StrToInt(temp[7]);
        8: Info.Score.Mate := StrToInt(temp[8]);
        9: Info.Score.LowerBound := True;
        10: Info.Score.UpperBound := True;
        11: Info.CurrMove := temp[11];
        12: Info.CurrMoveNumber := StrToInt(temp[12]);
        13: Info.HashFull := StrToInt(temp[13]);
        14: Info.NPS := StrToDWord(temp[14]);
        15: Info.TBHits := StrToInt(temp[15]);
        16: Info.SBHits := StrToInt(temp[16]);
        17: Info.CPULoad := StrToInt(temp[17]);
        18: Info.Str := temp[18];
        19: Info.Refutation := StringsToMoveList(Split(temp[19], ' '));
        20: Info.CurrLine := StringsToMoveList(Split(temp[20], ' '));
      end;
      InfoMask := InfoMask + [TInfoMaskElement(i)];
    end;
  FOnInfo(Self, Info, InfoMask);
end;

procedure TUCIEngine.ParseOption(const s: string);
const
  Tokens: array[1..6] of string = ('name', 'type', 'default', 'min', 'max', 'var');
var
  Option: TUCIOption;
  st: string;
  temp: TStringArray;
begin
  st := s;
  // Delete 'option' from beginning
  Delete(st, 1, 6);
  temp := GetValuesOfKeys(st, Tokens);
  case temp[1] of
    'check': Option := TCheckUCIOption.Create(temp[0], temp[2]);
    'spin': Option := TSpinUCIOption.Create(temp[0], temp[2], temp[3], temp[4]);
    'combo': Option := TComboUCIOption.Create(temp[0], temp[2], temp[5]);
    'button': Option := TButtonUCiOption.Create(temp[0]);
    'string': Option := TStringUCIOption.Create(temp[0], temp[2]);
  end;
  temp := nil;
  FOptions.Add(Option);
  {$IFDEF Logging}
  WriteLn('FOUND Option: ', Option.Name, ' ', Option.Typ);
  {$ENDIF}
end;

procedure TUCIEngine.ParseOutput;
var
  s: string;
  RegExpressions: array[1..8] of TRegExpr;
  i: byte;
begin
  for i := 1 to 8 do
    RegExpressions[i] := TRegExpr.Create;
  RegExpressions[1].Expression := '[ \t]*id[ \t].*';
  RegExpressions[2].Expression := '[ \t]*uciok[ \t]*';
  RegExpressions[3].Expression := '[ \t]*readyok[ \t]*';
  RegExpressions[4].Expression := '[ \t]*bestmove[ \t].*';
  RegExpressions[5].Expression := '[ \t]*copyprotection[ \t].*';
  RegExpressions[6].Expression := '[ \t]*registration[ \t].*';
  RegExpressions[7].Expression := '[ \t]*info[ \t].*';
  RegExpressions[8].Expression := '[ \t]*option[ \t].*';
  while Output.Count > 0 do
  begin
    s := Output.Strings[0];
    {$IFDEF Logging}
    WriteLn('PARSE: ', s);
    {$ENDIF}
    if RegExpressions[1].Exec(s) then
      ParseID(s);
    if RegExpressions[2].Exec(s) then
      UCIOk := True;
    if RegExpressions[3].Exec(s) then
    begin
      if Assigned(FOnReadyOk) then
        FOnReadyOk(Self);
    end;
    if RegExpressions[4].Exec(s) then
      ParseBestMove(s);
    if RegExpressions[5].Exec(s) then
      ParseCopyProtection(s);
    if RegExpressions[6].Exec(s) then
      ParseRegistration(s);
    if RegExpressions[7].Exec(s) then
      ParseInfo(s);
    if RegExpressions[8].Exec(s) then
      ParseOption(s);
    Output.Delete(0);
  end;
  for i := 1 to 8 do
    RegExpressions[i].Free;
end;

procedure TUCIEngine.ParseRegistration(const s: string);
var
  st: string;
  chk, err, ok: TRegExpr;
  Stat: TStatus;
begin
  if not Assigned(FOnRegistration) then
    Exit;
  st := s;
  chk := TRegExpr.Create;
  err := TRegExpr.Create;
  ok := TRegExpr.Create;
  chk.Expression := '[ \t]*registration[ \t]*checking[ \t]*';
  err.Expression := '[ \t]*registration[ \t]*error[ \t]*';
  ok.Expression := '[ \t]*registration[ \t]*ok[ \t]*';
  if chk.Exec(st) then
    Stat := stChecking;
  if err.Exec(st) then
    Stat := stError;
  if ok.Exec(st) then
    Stat := stOk;
  FOnRegistration(Self, Stat);
  FreeAndNil(chk);
  FreeAndNil(err);
  FreeAndNil(ok);
end;

procedure TUCIEngine.SendStr(msg: string);
begin
  msg := msg + LineEnding;
  FEngine.Input.Write(msg[1], length(msg));
  {$IFDEF Logging}
  Write('INPUT: ', msg);
  {$ENDIF}
end;

procedure TUCIEngine.SetDebug(AValue: boolean);
begin
  if FDebug = AValue then
    Quit;
  FDebug := AValue;
  if FDebug then
    Sendstr('debug on')
  else
    Sendstr('debug off');
end;

procedure TUCIEngine.SetProcessName(AValue: string);
begin
  if FPocessName = AValue then
    Exit;
  FPocessName := AValue;
end;

procedure TUCIEngine.WaitForToken(const Text: string; Interval: integer = 100);
var
  s: string;
  i, p, l: integer;
  TokenFound: boolean;
begin
  repeat
    Sleep(Interval);
    l := Output.Count;
    GetLine;
    TokenFound := False;
    for i := l to Output.Count - 1 do
    begin
      s := Trim(Output.Strings[i]);
      while Pos(' ', s) > 0 do
      begin
        p := Pos(' ', s);
        TokenFound := TokenFound or (Copy(s, 1, p - 1) = Text);
        Delete(s, 1, p);
      end;
      TokenFound := TokenFound or (s = Text);
    end;
  until TokenFound;
  ParseOutput;
end;

constructor TUCIEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDebug := False;
  FOptions := TUCIOptionList.Create;
  Output := TStringList.Create;
  Timer := TTimer.Create(Self);
  Timer := TTimer.Create(Self);
  Timer.Interval := 100;
  Timer.Enabled := False;
  Timer.OnTimer := @TimerTimer;
end;

destructor TUCIEngine.Destroy;
begin
  if Assigned(FEngine) then
  begin
    if isRunning then
      Quit;
    FreeAndNil(FEngine);
  end;
  FreeAndNil(FOptions);
  FreeAndNil(Timer);
  FreeAndNil(Output);
  inherited Destroy;
end;

procedure TUCIEngine.ApplyOption(Option: TUCIOption);
begin
  // TODO: Convert empty string '' in <empty> ?
  case Option.Typ of
    tpCheck: SendStr('setoption name ' + Option.Name + ' value ' +
        BoolToStr((Option as TCheckUCIOption).Value, 'true', 'false'));
    tpSpin: SendStr('setoption name ' + Option.Name + ' value ' +
        IntToStr((Option as TSpinUCIOption).Value));
    tpCombo: SendStr('setoption name ' + Option.Name + ' value ' +
        (Option as TComboUCIOption).Value);
    tpButton: SendStr('setoption name ' + Option.Name);
    tpString: SendStr('setoption name ' + Option.Name + ' value ' +
        (Option as TStringUCIOption).Value);
  end;
end;

procedure TUCIEngine.Go(searchmoves: TStringList; ponder: boolean;
  wtime: integer; btime: integer; winc: integer; binc: integer;
  movestogo: integer; depth: integer; nodes: integer; mate: integer;
  movetime: integer; infinite: boolean);
var
  Command: string;
  i: integer;
begin
  Command := 'go ';
  if Assigned(searchmoves) then
  begin
    Command := Command + 'searchmoves ';
    for i := 0 to searchmoves.Count - 1 do
      Command := Command + searchmoves.Strings[i] + ' ';
  end;
  if ponder then
    Command := Command + 'ponder ';
  if wtime > 0 then
    Command := Command + 'wtime ' + IntToStr(wtime) + ' ';
  if btime > 0 then
    Command := Command + 'btime ' + IntToStr(btime) + ' ';
  if winc > 0 then
    Command := Command + 'winc ' + IntToStr(winc) + ' ';
  if binc > 0 then
    Command := Command + 'binc ' + IntToStr(binc) + ' ';
  if movestogo >= 0 then
    Command := Command + 'movestogo ' + IntToStr(movestogo);
  if depth > 0 then
    Command := Command + 'depth ' + IntToStr(depth);
  if nodes > 0 then
    Command := Command + 'nodes ' + IntToStr(nodes);
  if mate > 0 then
    Command := Command + 'mate ' + IntToStr(mate);
  if movetime > 0 then
    Command := Command + 'movetime ' + IntToStr(movetime);
  if infinite then
    Command := Command + 'infinite';
  SendStr(Command);
  // WaitForToken('bestmove');
end;

procedure TUCIEngine.Init;
begin
  if Assigned(FEngine) then
    FEngine.Free;
  FEngine := TProcess.Create(Self);
  FEngine.Executable := self.ProcessName;
  FEngine.Options := [poUsePipes];
  FOptions.Clear;
  FEngine.Execute;
  GetLine;
  SendStr('uci');
  WaitForToken('uciok');
  Timer.Enabled := True;
  FisRunning := True;
  //Sleep(1000);
end;

procedure TUCIEngine.NewGame;
begin
  SendStr('ucinewgame');
  SendIsReady;
end;

procedure TUCIEngine.PonderHit;
begin
  SendStr('ponderhit');
end;

procedure TUCIEngine.Quit;
begin
  Timer.Enabled := False;
  FisRunning := False;
  SendStr('quit');
  if Assigned(FOnQuit) then
    FOnQuit(Self);
end;

procedure TUCIEngine.SendIsReady;
begin
  SendStr('isready');
end;

procedure TUCIEngine.SendRegistration(Later: boolean; AName, code: string);
begin
  if Later then
    SendStr('register later')
  else
    SendStr('register name ' + AName + ' code ' + code);
end;

procedure TUCIEngine.SetUpPosition(FEN: string; Moves: TMoveList);
var
  Move: TMove;
  msg: string;
begin
  if FEN <> 'startpos' then
    FEN := 'fen ' + FEN;
  if Assigned(Moves) then
  begin
    msg := 'position ' + FEN + ' moves ';
    for Move in Moves do
    begin
      msg := msg + AlgebraicMoveToString(Move);
      case Move.PromotionPiece of
        ptWKnight, ptBKnight: msg := msg + 'n';
        ptWBishop, ptBBishop: msg := msg + 'b';
        ptWRook, ptBRook: msg := msg + 'r';
        ptWQueen, ptBQueen: msg := msg + 'q';
      end;
      msg := msg + ' ';
    end;
    SendStr(msg);
  end
  else
    SendStr('position ' + FEN);
end;

procedure TUCIEngine.Stop;
begin
  SendStr('stop');
end;

end.
