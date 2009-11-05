(* The main window of the application. *)
UNIT UnitMainWindow;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, StdActns, ComCtrls, SynEdit, SynHighlighterPas;

TYPE

  { TMainWindow }

  TMainWindow = CLASS(TForm)
    ActionList: TActionList;
      EditCopy: TEditCopy;
      EditCut: TEditCut;
      EditPaste: TEditPaste;
      FileNew: TAction;
      FileOpen: TAction;
      FileSave: TAction;
      FileSaveAs: TFileSaveAs;
    MainMenu: TMainMenu;
      MenuItemFile: TMenuItem;
        MenuItemNew: TMenuItem;
        MenuItemOpen: TMenuItem;
        MenuItemSaveAs: TMenuItem;
        MenuItemSave: TMenuItem;
      MenuItemEdit: TMenuItem;
        MenuItemCut: TMenuItem;
        MenuItemCopy: TMenuItem;
        MenuItemPaste: TMenuItem;
      MenuItemExtra: TMenuItem;
        MenuItemAbout: TMenuItem;
        OpenDialog: TOpenDialog;
    SynEditPopupMenu: TPopupMenu;
      PopupMenuItemPaste: TMenuItem;
      PopupMenuItemCopy: TMenuItem;
      PopupMenuItemCut: TMenuItem;
    SynEdit: TSynEdit;
    SynPascalSyn: TSynPasSyn;
    StatusBar: TStatusBar;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE EditCopyExecute(Sender: TObject);
    PROCEDURE EditCutExecute(Sender: TObject);
    PROCEDURE EditPasteExecute(Sender: TObject);
    PROCEDURE FileOpenExecute(Sender: TObject);
    PROCEDURE MenuItemAboutClick(Sender: TObject);
    PROCEDURE FileNewExecute(Sender: TObject);
    PROCEDURE FileSaveAsAccept(Sender: TObject);
    PROCEDURE FileSaveExecute(Sender: TObject);
    PROCEDURE SynEditChange(Sender: TObject);
  PRIVATE
  (* Full file name. *)
    fFileName: STRING;

    PROCEDURE UpdateStatus;
  PUBLIC
    { public declarations }
  END;

VAR
  MainWindow: TMainWindow;

IMPLEMENTATION

USES
  UnitAboutDlg, SynEditKeyCmds, LCLType;



CONST
  TITLE_BASE = 'Z80-Pascal IDE 0.2.0';
{ Status bar panels. }
  MODIFIED_PANEL = 0;



{ TMainWindow }

(* Sets up the form. *)
  PROCEDURE TMainWindow.FormCreate(Sender: TObject);
  BEGIN
    SELF.FileNewExecute (FileNew);
  END;



(* Clean up the form. *)
  PROCEDURE TMainWindow.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  BEGIN
  { Checks if file was modified. }
    IF SynEdit.Modified THEN
      IF Application.MessageBox (
        'File was modified and closing you''ll lost the changes.'+
        #10+'Do you want to close?',
        TITLE_BASE, MB_ICONEXCLAMATION + MB_YESNO) = IDNO
      THEN
        CloseAction := caNone;
  END;



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



(* Traks changes. *)
  PROCEDURE TMainWindow.SynEditChange(Sender: TObject);
  BEGIN
    IF (Sender AS TSynEdit).Modified THEN
      UpdateStatus;
  END;



(* Copyes the selected text from the edit. *)
  PROCEDURE TMainWindow.EditCopyExecute(Sender: TObject);
  BEGIN
    SynEdit.CommandProcessor (TSynEditorCommand (ecCopy), '', NIL);
    UpdateStatus;
  END;



(* Cuts the selected text from the edit. *)
  PROCEDURE TMainWindow.EditCutExecute(Sender: TObject);
  BEGIN
    SynEdit.CommandProcessor (TSynEditorCommand (ecCut), '', NIL);
    UpdateStatus;
  END;



(* Pastes text to the edit. *)
  PROCEDURE TMainWindow.EditPasteExecute(Sender: TObject);
  BEGIN
    SynEdit.CommandProcessor (TSynEditorCommand (ecPaste), '', NIL);
    UpdateStatus;
  END;



(* Creates new file. *)
  PROCEDURE TMainWindow.FileNewExecute(Sender: TObject);
  BEGIN
  { Checks if current was modified. }
    IF SynEdit.Modified THEN
    BEGIN
      IF Application.MessageBox (
        'File was modified and creating a new one you''ll lost the changes.'+
        #10+'Do you want to create a new file?',
        TITLE_BASE, MB_ICONEXCLAMATION + MB_YESNO) = IDNO
      THEN
        EXIT;
    END;
  { Create a new file. }
    SynEdit.Lines.Text := '';
    SynEdit.Modified := FALSE;
    fFileName := '';
    UpdateStatus;
  END;




(* To open files. *)
  PROCEDURE TMainWindow.FileOpenExecute(Sender: TObject);
  BEGIN
  { Checks if current was modified. }
    IF SynEdit.Modified THEN
    BEGIN
      IF Application.MessageBox (
        'File was modified and opening a file you''ll lost the changes.'+
        #10+'Do you want to open a file?',
        TITLE_BASE, MB_ICONEXCLAMATION + MB_YESNO) = IDNO
      THEN
      (* Cancels the action. *)
        EXIT;
    END;
  { Gets the file. }
    IF OpenDialog.Execute THEN
    BEGIN
      SynEdit.Lines.LoadFromFile (OpenDialog.FileName);
      SynEdit.Modified := FALSE;
      fFileName := OpenDialog.FileName;
      UpdateStatus;
    END;
  END;



(* To save files. *)
  PROCEDURE TMainWindow.FileSaveExecute(Sender: TObject);
  BEGIN
    IF SELF.fFileName = '' THEN
      FileSaveAs.Execute
    ELSE
      SynEdit.Lines.SaveToFile (SELF.fFileName);
    UpdateStatus;
  END;




  PROCEDURE TMainWindow.FileSaveAsAccept(Sender: TObject);
  BEGIN
    SynEdit.Lines.SaveToFile (SELF.FileSaveAs.Dialog.FileName);
    fFileName := SELF.FileSaveAs.Dialog.FileName;
    UpdateStatus;
  END;



(* Updates the status. *)
  PROCEDURE TMainWindow.UpdateStatus;
  VAR
    CharModified, TextModified, FileName: STRING;
  BEGIN
    IF SynEdit.Modified THEN
    BEGIN
      CharModified := ' +';
      TextModified := 'Modified';
    END;
    FileName := ExtractFileName (fFileName);
    IF FileName = '' THEN
      FileName := '<unnamed>';
    SELF.Caption := FileName + CharModified + ' - ' + TITLE_BASE;
    StatusBar.Panels.Items[MODIFIED_PANEL].Text := TextModified;
  END;



INITIALIZATION
  {$I UnitMainWindow.lrs}

END.

