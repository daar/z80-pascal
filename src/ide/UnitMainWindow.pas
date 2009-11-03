(* The main window of the application. *)
unit UnitMainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, StdActns, SynEdit, SynHighlighterPas;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    ActionList: TActionList;
    OpenFile: TFileOpen;
    MainMenu: TMainMenu;
      MenuItemFile: TMenuItem;
        MenuItemNew: TMenuItem;
        MenuItemOpen: TMenuItem;
        MenuItemSaveAs: TMenuItem;
        MenuItemSave: TMenuItem;
      MenuItemAbout: TMenuItem;
    SaveDialog: TSaveDialog;
    SynEdit: TSynEdit;
    SynPascalSyn: TSynPasSyn;
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemNewClick(Sender: TObject);
    procedure MenuItemSaveAsClick(Sender: TObject);
    PROCEDURE OpenFileAccept(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainWindow: TMainWindow;

implementation

USES
  UnitAboutDlg;

{ TMainWindow }

(* To create a new file. *)
procedure TMainWindow.MenuItemNewClick(Sender: TObject);
begin
  SynEdit.Lines.Text := '';
  SynEdit.Modified := FALSE;
end;



(* Shows the "About..." dialog. *)
procedure TMainWindow.MenuItemAboutClick(Sender: TObject);
VAR
  Dialog: TForm;
begin
  Dialog := TAboutDialog.Create (Self);
  TRY
    Dialog.ShowModal;
  FINALLY
    Dialog.Free;
  END;
end;



(* To save the file. *)
procedure TMainWindow.MenuItemSaveAsClick(Sender: TObject);
begin
  IF SaveDialog.Execute THEN
  BEGIN
    SynEdit.Lines.SaveToFile (SaveDialog.FileName);
  END;
end;



(* To open files. *)
PROCEDURE TMainWindow.OpenFileAccept(Sender: TObject);
BEGIN
  SynEdit.Lines.LoadFromFile (SELF.OpenFile.Dialog.FileName);
end;

initialization
  {$I UnitMainWindow.lrs}

end.

