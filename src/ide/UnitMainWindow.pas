(* The main window of the application. *)
UNIT UnitMainWindow;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, StdActns, SynEdit, SynHighlighterPas;

TYPE

  { TMainWindow }

  TMainWindow = CLASS(TForm)
    ActionList: TActionList;
      FileNew: TAction;
      FileSave: TAction;
      FileSaveAs: TFileSaveAs;
      OpenFile: TFileOpen;
    MainMenu: TMainMenu;
      MenuItemFile: TMenuItem;
        MenuItemNew: TMenuItem;
        MenuItemOpen: TMenuItem;
        MenuItemSaveAs: TMenuItem;
        MenuItemSave: TMenuItem;
      MenuItemAbout: TMenuItem;
    SynEdit: TSynEdit;
    SynPascalSyn: TSynPasSyn;
    PROCEDURE MenuItemAboutClick(Sender: TObject);
    PROCEDURE FileNewExecute(Sender: TObject);
    PROCEDURE OpenFileAccept(Sender: TObject);
    PROCEDURE FileSaveAsAccept(Sender: TObject);
    PROCEDURE FileSaveExecute(Sender: TObject);
  PRIVATE
  (* Full file name. *)
    fFileName: STRING;
  PUBLIC
    { public declarations }
  END;

VAR
  MainWindow: TMainWindow;

IMPLEMENTATION

USES
  UnitAboutDlg;

{ TMainWindow }




(* Shows the "About..." dialog. *)
  PROCEDURE TMainWindow.MenuItemAboutClick(Sender: TObject);
  VAR
    Dialog: TForm;
  BEGIN
    Dialog := TAboutDialog.Create (Self);
    TRY
      Dialog.ShowModal;
    FINALLY
      Dialog.Free;
    END;
  END;




(* Creates new file. *)
  PROCEDURE TMainWindow.FileNewExecute(Sender: TObject);
  BEGIN
    SynEdit.Lines.Text := '';
    SynEdit.Modified := FALSE;
    SELF.fFileName := '';
  END;



(* To open files. *)
  PROCEDURE TMainWindow.OpenFileAccept(Sender: TObject);
  BEGIN
    SynEdit.Lines.LoadFromFile (SELF.OpenFile.Dialog.FileName);
  END;



(* To save files. *)
  PROCEDURE TMainWindow.FileSaveExecute(Sender: TObject);
  BEGIN
    IF SELF.fFileName = '' THEN
      FileSaveAs.ExecuteTarget (Sender)
    ELSE
      SynEdit.Lines.SaveToFile (SELF.fFileName);
  END;

  PROCEDURE TMainWindow.FileSaveAsAccept(Sender: TObject);
  BEGIN
    SynEdit.Lines.SaveToFile (SELF.FileSaveAs.Dialog.FileName);
    SELF.fFileName := SELF.FileSaveAs.Dialog.FileName;
  END;



INITIALIZATION
  {$I UnitMainWindow.lrs}

END.

