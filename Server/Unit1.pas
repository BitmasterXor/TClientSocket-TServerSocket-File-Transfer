unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Win.ScktComp, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TTransferMode = (tmNone, tmSending, tmReceiving);

  TForm1 = class(TForm)
    ServerSocket1: TServerSocket;
    ProgressBar1: TProgressBar;
    lblStatus: TLabel;
    btnSendFile: TButton;
    OpenDialog1: TOpenDialog;
    procedure ServerSocket1ClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure ServerSocket1ClientConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ServerSocket1ClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure FormCreate(Sender: TObject);
    procedure btnSendFileClick(Sender: TObject);
  private
    FStream: TFileStream;
    FFileName: string;
    FFileSize: Int64;
    FTransferredBytes: Int64;
    FReceivingFileInfo: Boolean;
    FFileInfo: string;
    FTransferMode: TTransferMode;

    procedure ProcessFileInfo(const FileInfo: string);
    procedure UpdateProgress;
    procedure SendFileChunk(Socket: TCustomWinSocket);
    procedure CleanupTransfer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  CHUNK_SIZE = 8192; // 8KB chunks for sending

procedure TForm1.FormCreate(Sender: TObject);
begin
  ServerSocket1.Port := 3434;
  ServerSocket1.Active := True;
  FReceivingFileInfo := True;
  FFileInfo := '';
  FTransferMode := tmNone;
  lblStatus.Caption := 'Server started. Waiting for client connection...';
  btnSendFile.Enabled := False;
end;

procedure TForm1.btnSendFileClick(Sender: TObject);
begin
  // Only allow sending one file at a time
  if FTransferMode <> tmNone then
  begin
    ShowMessage('Already transferring a file. Please wait.');
    Exit;
  end;

  // Select the file to send
  if OpenDialog1.Execute then
  begin
    try
      // Initialize sending process
      FFileName := OpenDialog1.FileName;
      FStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
      FFileSize := FStream.Size;
      FTransferredBytes := 0;
      FTransferMode := tmSending;

      // Initialize progress bar
      ProgressBar1.Position := 0;
      lblStatus.Caption := 'Sending: ' + ExtractFileName(FFileName);

      // First, send the file info to the client with a flag indicating server is sending
      if ServerSocket1.Socket.ActiveConnections > 0 then
        ServerSocket1.Socket.Connections[0].SendText(
          'SERVER_SENDING|' + IntToStr(FFileSize) + '|' + ExtractFileName(FFileName)
        )
      else
      begin
        ShowMessage('No connected clients.');
        CleanupTransfer;
      end;
    except
      on E: Exception do
      begin
        ShowMessage('Error starting file transfer: ' + E.Message);
        CleanupTransfer;
      end;
    end;
  end;
end;

procedure TForm1.CleanupTransfer;
begin
  if Assigned(FStream) then
    FreeAndNil(FStream);
  FTransferMode := tmNone;
end;

procedure TForm1.ProcessFileInfo(const FileInfo: string);
var
  Parts: TArray<string>;
  SavePath: string;
begin
  // Check if this is a SERVER_SENDING message from another server
  if FileInfo.StartsWith('SERVER_SENDING|') then
  begin
    ShowMessage('Error: Received a server file send request from client. Ignoring.');
    FReceivingFileInfo := True;
    FFileInfo := '';
    Exit;
  end;

  // Parse file info (format: "FileSize|FileName")
  Parts := FileInfo.Split(['|']);

  if Length(Parts) >= 2 then
  begin
    try
      FFileSize := StrToInt64(Parts[0]);
      FFileName := Parts[1];
      FTransferredBytes := 0;
      FTransferMode := tmReceiving;

      // Create the file stream
      SavePath := ExtractFilePath(Application.ExeName) + FFileName;

      if FileExists(SavePath) and
         (MessageDlg('File already exists. Overwrite?', mtConfirmation, [mbYes, mbNo], 0) = mrNo) then
      begin
        lblStatus.Caption := 'File transfer cancelled.';
        FTransferMode := tmNone;
        Exit;
      end;

      // Create file stream
      FStream := TFileStream.Create(SavePath, fmCreate);
      lblStatus.Caption := 'Receiving file: ' + FFileName;

      // Ready to receive file data
      FReceivingFileInfo := False;

      // Tell client we're ready for file data
      ServerSocket1.Socket.Connections[0].SendText('READY_TO_RECEIVE');
    except
      on E: Exception do
      begin
        ShowMessage('Error preparing to receive file: ' + E.Message);
        CleanupTransfer;
      end;
    end;
  end
  else
  begin
    ShowMessage('Invalid file information received from client.');
    FTransferMode := tmNone;
  end;
end;

procedure TForm1.UpdateProgress;
var
  PercentComplete: Integer;
begin
  if FFileSize > 0 then
  begin
    PercentComplete := Round((FTransferredBytes / FFileSize) * 100);
    ProgressBar1.Position := PercentComplete;

    if FTransferMode = tmSending then
      lblStatus.Caption := Format('Sending: %s (%d%%)',
        [ExtractFileName(FFileName), PercentComplete])
    else
      lblStatus.Caption := Format('Receiving: %s (%d%%)',
        [FFileName, PercentComplete]);

    // Check if the transfer is complete
    if FTransferredBytes >= FFileSize then
    begin
      if FTransferMode = tmSending then
        lblStatus.Caption := 'File sent successfully!'
      else
        lblStatus.Caption := 'File received successfully!';

      if FTransferMode = tmReceiving then
      begin
        // Reset for the next transfer
        FReceivingFileInfo := True;
        FFileInfo := '';
      end;

      CleanupTransfer;
    end;
  end;
end;

procedure TForm1.SendFileChunk(Socket: TCustomWinSocket);
var
  Buffer: array[0..CHUNK_SIZE-1] of Byte;
  BytesToSend, BytesRead: Integer;
begin
  if (FTransferMode <> tmSending) or not Assigned(FStream) then
    Exit;

  try
    // Determine how many bytes to send in this chunk
    BytesToSend := CHUNK_SIZE;
    if FTransferredBytes + BytesToSend > FFileSize then
      BytesToSend := FFileSize - FTransferredBytes;

    if BytesToSend <= 0 then
    begin
      // We're done sending the file
      lblStatus.Caption := 'File sent successfully!';
      CleanupTransfer;
      Exit;
    end;

    // Read from file
    BytesRead := FStream.Read(Buffer, BytesToSend);
    if BytesRead > 0 then
    begin
      // Send chunk to client
      Socket.SendBuf(Buffer, BytesRead);

      // Update progress
      Inc(FTransferredBytes, BytesRead);
      UpdateProgress;

      // Use Windows message to allow UI to update
      Application.ProcessMessages;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Error during file transfer: ' + E.Message);
      CleanupTransfer;
    end;
  end;
end;

procedure TForm1.ServerSocket1ClientConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  lblStatus.Caption := 'Client connected. Ready for file transfer.';
  btnSendFile.Enabled := True;
  FReceivingFileInfo := True;
  FFileInfo := '';
  FTransferMode := tmNone;
  ProgressBar1.Position := 0;
end;

procedure TForm1.ServerSocket1ClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  btnSendFile.Enabled := False;

  if FTransferMode <> tmNone then
  begin
    CleanupTransfer;
    ShowMessage('Client disconnected during file transfer.');
    ProgressBar1.Position := 0;
  end;

  lblStatus.Caption := 'Client disconnected.';
end;

procedure TForm1.ServerSocket1ClientRead(Sender: TObject; Socket: TCustomWinSocket);
var
  Buffer: TBytes;
  DataSize: Integer;
  ReceivedText: string;
begin
  if FTransferMode = tmSending then
  begin
    // We're sending a file, expect acknowledgments
    ReceivedText := Socket.ReceiveText;

    if (ReceivedText = 'READY_TO_RECEIVE') or (ReceivedText = 'CHUNK_RECEIVED') then
      SendFileChunk(Socket);
  end
  else if FReceivingFileInfo then
  begin
    // Still collecting file info for receiving
    FFileInfo := FFileInfo + Socket.ReceiveText;

    // Check if we have the complete file info (contains a pipe)
    if Pos('|', FFileInfo) > 0 then
      ProcessFileInfo(FFileInfo);
  end
  else if FTransferMode = tmReceiving then
  begin
    // Receiving file data
    try
      DataSize := Socket.ReceiveLength;

      if DataSize > 0 then
      begin
        SetLength(Buffer, DataSize);
        Socket.ReceiveBuf(Buffer[0], DataSize);

        // Write to file
        if Assigned(FStream) then
        begin
          FStream.WriteBuffer(Buffer[0], DataSize);
          Inc(FTransferredBytes, DataSize);
          UpdateProgress;

          // Acknowledge receipt of chunk
          Socket.SendText('CHUNK_RECEIVED');
        end;
      end;
    except
      on E: Exception do
      begin
        ShowMessage('Error receiving file data: ' + E.Message);
        CleanupTransfer;
      end;
    end;
  end;
end;

end.
