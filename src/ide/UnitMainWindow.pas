(* The main window of the application. *)
unit UnitMainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, SynEdit, SynHighlighterPas;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    MainMenu: TMainMenu;
      MenuItemFile: TMenuItem;
        MenuItemNew: TMenuItem;
        MenuItemOpen: TMenuItem;
        MenuItemSaveAs: TMenuItem;
        MenuItemSave: TMenuItem;
      MenuItemAbout: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SynEdit: TSynEdit;
    SynPascalSyn: TSynPasSyn;
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemNewClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure MenuItemSaveAsClick(Sender: TObject);
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



(* To open files. *)
procedure TMainWindow.MenuItemOpenClick(Sender: TObject);
begin
  IF OpenDialog.Execute THEN
  BEGIN
    SynEdit.Lines.LoadFromFile (OpenDialog.FileName);
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

initialization
  {$I UnitMainWindow.lrs}

end.

