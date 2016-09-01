{ JCC (Jan's Chess Componenents) - This file contains
  Copyright (C) 2016  Jan Dette

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

unit Game;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MoveList, Board, Position, Ply;

type

  { TGame }

  TGame = class
  private
    FBoard: TBoard;
    //  FCurrentPlyNumber: word;
    FCurrentPlyNode: TPlyTreeNode;
    //  FPlyList: TPlyList;
    FPlyTree: TPlyTree;
    StartPosition: TPosition;
    function GetCurrentPlyNumber: word;
  protected
    function GetNotation: string; virtual;
  public
    // Simply adds a move at current position
    procedure AddMove(AMove: TMove);
    // AMove will become part of main line and the current main line will
    // be kept as side line
    procedure AddMoveAsNewMainLine(AMove: TMove);
    // AMove will be the start of a new side line
    procedure AddMoveAsSideLine(AMove: TMove);
    constructor Create(ABoard: TBoard); virtual;
    procedure Clear;
    destructor Destroy; override;
    // Setups position before last move on board
    procedure GoOneMoveBackward;
    // Setups position after next move on board
    procedure GoOneMoveForward;
    // Setups position after the given ply number (0 is then the initial position)
    procedure GoToPositionAfterPlyTreeNode(const APlyTreeNode: TPlyTreeNode);
    // Inserts a move at the current ply number as new variation
    procedure InsertAsNewVariation(AMove: TMove); deprecated;
    // Inserts a move at the current ply number in the same variation
    procedure InsertAsSubVariation(AMove: TMove); deprecated;
    // Converts the given move into a by human readable string
    function MoveToString(AMove: TMove): string; virtual; abstract; deprecated;
    // Similiar to AddMoveAsNewMainLine but current main line will be deleted
    procedure ReplaceMainLine(AMove: TMove);
  public
    property CurrentPlyNumber: word read GetCurrentPlyNumber;
    property CurrentPlyNode: TPlyTree.TTreeNodeType read FCurrentPlyNode;
    property Notation: string read GetNotation;
    //  property PlyList: TPlyList read FPlyList;
    // NOTE: PlyTree.Root.Data will always be nil
    property PlyTree: TPlyTree read FPlyTree;
  end;

  { TStandardGame }

  TStandardGame = class(TGame)
  protected
    function GetNotation: string; override;
  public
    constructor Create(ABoard: TBoard); override;
    function MoveToString(AMove: TMove): string; override; deprecated;
  end;

implementation

procedure DeletePly(const AData: TPly);
begin
  AData.Free;
end;

{ TGame }

function TGame.GetCurrentPlyNumber: word;
var
  Temp: TPlyTreeNodeList;
begin
  Temp := FPlyTree.GetPathTo(FCurrentPlyNode);
  Result := Temp.Size - 1;
  Temp.Free;
end;

function TGame.GetNotation: string;
begin
  Result := '';
end;

procedure TGame.AddMove(AMove: TMove);
begin
  if FPlyTree.Root.Children.Size > 0 then
  begin
    // FPlyList.Add(TPly.Create(AMove, FPlyList.Last.VariationLevel));
    FCurrentPlyNode.Children.PushBack(TPlyTreeNode.Create(TPly.Create(AMove)));
    FCurrentPlyNode := FCurrentPlyNode.Children.Items[0];
  end
  else
  begin
    // First move is done, so we expect this to be the start position
    StartPosition.Copy(FBoard.CurrentPosition);
    // FPlyList.Add(TPly.Create(AMove, 0));
    FCurrentPlyNode.Children.PushBack(TPlyTreeNode.Create(TPly.Create(AMove)));
    FCurrentPlyNode := FCurrentPlyNode.Children.Items[0];
    // FPlyTree.Root := TPlyTreeNode.Create(TPly.Create(AMove));
  end;
  // Inc(FCurrentPlyNumber);
  FBoard.CurrentPosition.PlayMove(AMove);
end;

procedure TGame.AddMoveAsNewMainLine(AMove: TMove);
begin
  FCurrentPlyNode.Children.Insert(0, TPlyTreeNode.Create(TPly.Create(AMove)));
  FCurrentPlyNode := FCurrentPlyNode.Children.Items[0];
  FBoard.CurrentPosition.PlayMove(AMove);
end;

procedure TGame.AddMoveAsSideLine(AMove: TMove);
begin
  FCurrentPlyNode.Children.PushBack(TPlyTreeNode.Create(TPly.Create(AMove)));
  FCurrentPlyNode := FCurrentPlyNode.Children.Back;
  FBoard.CurrentPosition.PlayMove(AMove);
end;

constructor TGame.Create(ABoard: TBoard);
begin
  // FCurrentPlyNumber := 0;
  FBoard := ABoard;
  StartPosition := TStandardPosition.Create;
  // FPlyList := TPlyList.Create(True);
  FPlyTree := TPlyTree.Create;
  FPlyTree.Root := TPlyTreeNode.Create(nil);
  FCurrentPlyNode := FPlyTree.Root;
end;

procedure TGame.Clear;
begin
  // FCurrentPlyNumber := 0;
  // FPlyList.Clear;
  // We need to delete all plies currently stored in the tree
  FPlyTree.DepthFirstTraverse(@DeletePly);
  FPlyTree.Root.Free;
  FPlyTree.Root := TPlyTreeNode.Create(nil);
  FCurrentPlyNode := FPlyTree.Root;
end;

destructor TGame.Destroy;
begin
  // FPlyList.Free;
  // We need to delete all plies currently stored in the tree
  FPlyTree.DepthFirstTraverse(@DeletePly);
  FPlyTree.Free;
  StartPosition.Free;
  inherited Destroy;
end;

procedure TGame.GoOneMoveBackward;
begin
  GoToPositionAfterPlyTreeNode(FPlyTree.GetParentOf(FCurrentPlyNode));
end;

procedure TGame.GoOneMoveForward;
begin
  GoToPositionAfterPlyTreeNode(FCurrentPlyNode.Children.Items[0]);
end;

procedure TGame.GoToPositionAfterPlyTreeNode(const APlyTreeNode: TPlyTreeNode);
var
  Temp: TPlyTreeNodeList;
  Node: TPlyTreeNode;
begin
  Temp := FPlyTree.GetPathTo(APlyTreeNode);
  if Temp = nil then
    // if Index > FPlyList.Count then
    raise Exception.Create('The given node is not in the tree');
  FBoard.CurrentPosition.Copy(StartPosition);
  for Node in Temp do
  begin
    if Node.Data <> nil then
      FBoard.CurrentPosition.PlayMove(Node.Data.Move);
  end;
  temp.Free;
  // FCurrentPlyNumber := Index;
  FCurrentPlyNode := APlyTreeNode;
  FBoard.Invalidate;
end;

procedure TGame.InsertAsNewVariation(AMove: TMove);
begin
  FCurrentPlyNode.Children.PushBack(TPlyTreeNode.Create(TPly.Create(AMove)));
  FCurrentPlyNode := FCurrentPlyNode.Children.Back;
  FBoard.CurrentPosition.PlayMove(AMove);
end;

procedure TGame.InsertAsSubVariation(AMove: TMove);
begin
  FBoard.CurrentPosition.PlayMove(AMove);
end;

procedure TGame.ReplaceMainLine(AMove: TMove);
begin
  FCurrentPlyNode.Children.Erase(0);
  FCurrentPlyNode.Children.Insert(0, TPlyTreeNode.Create(TPly.Create(AMove)));
  FCurrentPlyNode := CurrentPlyNode.Children.Items[0];
  FBoard.CurrentPosition.PlayMove(AMove);
end;

{ TStandardGame }

function TStandardGame.GetNotation: string;
var
  VarLevel: word;

  function RecursiveLineToString(CurrentRoot: TPlyTreeNode;
    StartPos: TStandardPosition): string;
  var
    TempPos: TStandardPosition;

    function PlyToStr(APly: TPly): string;
    begin
      if TempPos.WhitesTurn then
        Result := (IntToStr(TempPos.MoveNumber) + '.' +
          TempPos.MoveToSAN(APly.Move))
      else
        Result := ' ' + TempPos.MoveToSAN(APly.Move) + ' ';
    end;

  var
    Ply: TPly;
    i: integer;
  begin
    Result := '';
    if CurrentRoot.Children.Size = 0 then
    begin // End of line
      if VarLevel = 1 then
        Result := '] ' + LineEnding
      else
      if VarLevel > 1 then
        Result := ') ';
      Exit;
    end;
    TempPos := TStandardPosition.Create;
    TempPos.Copy(StartPos);
    Ply := CurrentRoot.Children.Items[0].Data;
    // Write first move of main line
    Result := Result + PlyToStr(Ply);
    // TODO: Brackets and dots like ..Nf6 are missing
    if CurrentRoot.Children.Size > 1 then
    begin
      Inc(VarLevel);
      for i := 1 to CurrentRoot.Children.Size - 1 do
      begin
        TempPos.Copy(StartPos);
        Ply := CurrentRoot.Children.Items[i].Data;
        // Write side lines
        if VarLevel = 1 then
          Result := Result + LineEnding + StringOfChar(' ', 2 * VarLevel) + '['
        else
        if VarLevel > 1 then
          Result := Result + '(';
        if TempPos.WhitesTurn then
          Result := Result + IntToStr(TempPos.MoveNumber) + '.'
        else
          Result := Result + IntToStr(TempPos.MoveNumber) + '...';
        Result := Result + TempPos.MoveToSAN(Ply.Move) + ' ';
        TempPos.PlayMove(Ply.Move);
        Result := Result + RecursiveLineToString(CurrentRoot.Children.Items[i], TempPos);
      end;
      Dec(VarLevel);
    end;
    TempPos.Copy(StartPos);
    Ply := CurrentRoot.Children.Items[0].Data;
    TempPos.PlayMove(Ply.Move);
    // Write t rest of main line
    Result := Result + RecursiveLineToString(CurrentRoot.Children.Items[0], TempPos);
    TempPos.Free;
  end;

begin
  Result := inherited GetNotation;
  if FPlyTree.Count > 0 then
  begin
    Varlevel := 0;
    // TODO: Support for variations
    Result := RecursiveLineToString(FPlyTree.Root, StartPosition as TStandardPosition);
  end;
end;

constructor TStandardGame.Create(ABoard: TBoard);
begin
  inherited Create(ABoard);
  FBoard.CurrentPosition := TStandardPosition.Create;
end;

function TStandardGame.MoveToString(AMove: TMove): string;
begin
  Result := TStandardPosition(FBoard.CurrentPosition).MoveToSAN(AMove);
end;

end.
