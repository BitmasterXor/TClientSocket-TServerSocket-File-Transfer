unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Win.ScktComp, Vcl.ComCtrls;

type
  TTransferMode = (tmNone, tmSending, tmReceiving);

  TForm1 = class(TForm)
    ClientSocket1: TClientSocket;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    ProgressBar1: TProgressBar;
    lblStatus: TLabel;
    btnSendFile: TButton;
    procedure Button1Click(Sender: TObject);
    procedure ClientSocket1Connect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ClientSocket1Disconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ClientSocket1Read(Sender: TObject; Socket: TCustomWinSocket);
    procedure btnSendFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FStream: TFileStream;
    FFileName: string;
    FFileSize: Int64;
    FTransferredBytes: Int64;
    FTransferMode: TTransferMode;
    FReceivingFileInfo: Boolean;
    FFileInfo: string;

    procedure UpdateProgress;
    procedure SendFileChunk;
    procedure ProcessFileInfo(const FileInfo: string);
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
  FTransferMode := tmNone;
  FReceivingFileInfo := True;
  FFileInfo := '';
  btnSendFile.Enabled := False;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if ClientSocket1.Active then
  begin
    ClientSocket1.Active := False;
    Button1.Caption := 'Connect';
    lblStatus.Caption := 'Disconnected';
    btnSendFile.Enabled := False;
  end
  else
  begin
    try
      ClientSocket1.Host := 'localhost'; // Change to server IP if needed
      ClientSocket1.Port := 3434;        // Change to server port if needed
      ClientSocket1.Active := True;
      Button1.Caption := 'Disconnect';
    except
      on E: Exception do
        ShowMessage('Connection error: ' + E.Message);
    end;
  end;
end;

procedure TForm1.btnSendFileClick(Sender: TObject);
begin
  // Only allow sending one file at a time
  if FTransferMode <> tmNone then
  begin
    ShowMessage('Already in a file transfer. Please wait.');
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

      // Send file info to the server
      ClientSocket1.Socket.SendText(
        IntToStr(FFileSize) + '|' + ExtractFileName(FFileName)
      );
    except
      on E: Exception do
      begin
        ShowMessage('Error starting file transfer: ' + E.Message);
        CleanupTransfer;
      end;
    end;
  end;
end;

procedure TForm1.ClientSocket1Connect(Sender: TObject; Socket: TCustomWinSocket);
begin
  lblStatus.Caption := 'Connected to server. Ready for file transfer.';
  btnSendFile.Enabled := True;
  FReceivingFileInfo := True;
  FFileInfo := '';
  FTransferMode := tmNone;
  ProgressBar1.Position := 0;
end;

procedure TForm1.ClientSocket1Disconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  Button1.Caption := 'Connect';
  lblStatus.Caption := 'Disconnected';
  btnSendFile.Enabled := False;
  CleanupTransfer;
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
  Prefix: string;
begin
  Prefix := '';
  if FileInfo.StartsWith('SERVER_SENDING|') then
    Prefix := 'SERVER_SENDING|';

  // Parse file info (format: ["SERVER_SENDING|"]FileSize|FileName)
  Parts := FileInfo.Substring(Length(Prefix)).Split(['|']);

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

      // Tell server we're ready for file data
      ClientSocket1.Socket.SendText('READY_TO_RECEIVE');
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
    ShowMessage('Invalid file information received.');
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

procedure TForm1.SendFileChunk;
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
      // Send chunk to server
      ClientSocket1.Socket.SendBuf(Buffer, BytesRead);

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

procedure TForm1.ClientSocket1Read(Sender: TObject; Socket: TCustomWinSocket);
var
  Response: string;
  Buffer: TBytes;
  DataSize: Integer;
begin
  if FTransferMode = tmSending then
  begin
    // We're sending a file, expect acknowledgments
    Response := Socket.ReceiveText;

    if Response = 'CHUNK_RECEIVED' then
    begin
      // Server acknowledged receipt, send next chunk if not done
      if Assigned(FStream) and (FTransferredBytes < FFileSize) then
        SendFileChunk;
    end
    else if Response = 'READY_TO_RECEIVE' then
    begin
      // Server is ready to receive file data, start sending
      SendFileChunk;
    end;
  end
  else if FReceivingFileInfo then
  begin
    // Check if we're receiving file info
    FFileInfo := FFileInfo + Socket.ReceiveText;

    // Check if we have the complete file info (contains a pipe)
    if Pos('|', FFileInfo) > 0 then
      ProcessFileInfo(FFileInfo);
  end
  else if FTransferMode = tmReceiving then
  begin
    // Receiving file data
    try
      // Get received data
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
