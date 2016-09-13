{ JCC (Jan's Chess Componenents) - This file contains classes to save and handle plies
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

unit Ply;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gtree, gstack, MoveList;

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
    constructor Create(const AMove: TMove; AVariationLevel: word = 0; ANAG: byte = 0);
    destructor Destroy; override;
  public
    property CommenTextInBehind: string read FCommenTextInBehind
      write FCommenTextInBehind;
    property CommentTextInFront: string read FCommentTextInFront
      write FCommentTextInFront;
    property Move: TMove read FMove write FMove;
    property NAG: byte read FNAG write FNAG;
    property VariationLevel: word read FVariationLevel write FVariationLevel;
  end;

  TPlyTree = specialize TTree<TPly>;
  TPlyTreeNode = TPlyTree.TTreeNodeType;
  TPlyTreeNodeList = TPlyTreeNode.TTreeNodeList;
  TPlyTreeNodeStack  = specialize TStack<TPlyTreeNode>;

  { TPlyTreeHelper }

  TPlyTreeHelper = class helper for TPlyTree
    // Counts the nodes in the tree
    function Count: word;
    // Returns the parent node of a given node
    function GetParentOf(const APlyTreeNode: TPlyTreeNode): TPlyTreeNode;
    // Returns the path in the tree to the given node
    function GetPathTo(const APlyTreeNode: TPlyTreeNode): TPlyTreeNodeList;
  end;

implementation

{ TPlyTreeHelper }

function TPlyTreeHelper.Count: word;

  function RecursiveCount(CurrentNode: TPlyTreeNode): word;
  var
    Child: TPlyTreeNode;
  begin
    Result := 0;
    if CurrentNode = nil then
      Exit;
    for Child in CurrentNode.Children do
    begin
      Result := Result + RecursiveCount(Child);
    end;
    Result := Result + 1;
  end;

begin
  Result := RecursiveCount(Self.Root);
end;

function TPlyTreeHelper.GetParentOf(const APlyTreeNode: TPlyTreeNode): TPlyTreeNode;

  function RecursiveSearch(CurrentRoot: TPlyTreeNode): TPlyTreeNode;
  var
    Child: TPlyTreeNode;
  begin
    Result := nil;
    if (CurrentRoot = nil) then
      Exit;
    for Child in CurrentRoot.Children do
    begin
      if Child = APlyTreeNode then
        Result := CurrentRoot;
    end;
    if Result = nil then
    begin
      for Child in CurrentRoot.Children do
      begin
        Result := RecursiveSearch(Child);
        if Result <> nil then
          Break;
      end;
    end;
  end;

begin
  Result := RecursiveSearch(Self.Root);
end;

function TPlyTreeHelper.GetPathTo(const APlyTreeNode: TPlyTreeNode): TPlyTreeNodeList;

  function RecursiveSearch(CurrentRoot: TPlyTreeNode;
    CurrentPath: TPlyTreeNodeList): boolean;
  var
    Child: TPlyTreeNode;
  begin
    Result := False;
    if CurrentRoot = nil then
      Exit;
    if CurrentRoot = APlyTreeNode then
    begin
      CurrentPath.Insert(0, CurrentRoot);
      Result := True;
      Exit;
    end;
    for Child in CurrentRoot.Children do
    begin
      if RecursiveSearch(Child, CurrentPath) then
      begin
        CurrentPath.Insert(0, CurrentRoot);
        Result := True;
        Exit;
      end;
    end;
  end;

begin
  Result := TPlyTreeNodeList.Create;
  RecursiveSearch(Self.Root, Result);
end;

{ TPly }

constructor TPly.Create(const AMove: TMove; AVariationLevel: word; ANAG: byte);
begin
  FMove := AMove;
  FVariationLevel := AVariationLevel;
  FNAG := ANAG;
end;

destructor TPly.Destroy;
begin
  inherited Destroy;
end;

end.
