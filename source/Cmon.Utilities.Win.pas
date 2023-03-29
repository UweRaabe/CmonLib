unit Cmon.Utilities.Win;

interface

uses
  Winapi.Windows,
  Cmon.Utilities;

type
  TVersionInfo = record
  private
    FBuildVersion: Integer;
    FCompany: string;
    FCopyright: string;
    FInfo: string;
    FIsValid: Boolean;
    FMajorVersion: Integer;
    FMinorVersion: Integer;
    FProduct: string;
    FReleaseVersion: Integer;
    FVersion: string;
    FVersionName: string;
  public
    property BuildVersion: Integer read FBuildVersion;
    property Company: string read FCompany;
    property Copyright: string read FCopyright;
    property Info: string read FInfo;
    property IsValid: Boolean read FIsValid;
    property MajorVersion: Integer read FMajorVersion;
    property MinorVersion: Integer read FMinorVersion;
    property Product: string read FProduct;
    property ReleaseVersion: Integer read FReleaseVersion;
    property Version: string read FVersion;
    property VersionName: string read FVersionName;
  end;

type
  TUtilitiesHelper = record helper for TUtilities
  private
    class function GetComputerName: string; static;
    class function GetComputerNameEx(NameFormat: TComputerNameFormat): string; static;
    class function GetUserName: string; static;
    class function GetVersionInfo: TVersionInfo; overload; static;
  public
    class function GetVersionInfo(const AFileName: string): TVersionInfo; overload; static;
    class property ComputerName: string read GetComputerName;
    class property ComputerNameEx[NameFormat: TComputerNameFormat]: string read GetComputerNameEx;
    class property UserName: string read GetUserName;
    class property VersionInfo: TVersionInfo read GetVersionInfo;
  end;

implementation

uses
  System.SysUtils;

class function TUtilitiesHelper.GetComputerName: string;
var
  buffer: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  size: DWORD;
begin
  Result := '';
  size := MAX_COMPUTERNAME_LENGTH + 1;
  if (Winapi.Windows.GetComputerName(buffer, size)) then
    Result := buffer;
end;

class function TUtilitiesHelper.GetComputerNameEx(NameFormat: TComputerNameFormat): string;
var
  buffer: array of Char;
  size: DWORD;
begin
  Result := '';
  Winapi.Windows.GetComputerNameEx(NameFormat, nil, size);
  SetLength(buffer, size);
  if (Winapi.Windows.GetComputerNameEx(NameFormat, PChar(buffer), size)) then
    Result := PChar(buffer);
end;

class function TUtilitiesHelper.GetUserName: string;
const
  cMaxUserNameLength = 256;
var
  buffer: array[0..cMaxUserNameLength] of Char;
  size: DWORD;
begin
  Result := '';
  size := cMaxUserNameLength + 1;
  if (Winapi.Windows.GetUserName(buffer, size)) then
    Result := buffer;
end;

class function TUtilitiesHelper.GetVersionInfo(const AFileName: string): TVersionInfo;
type
  TTranslation = array[0 .. 1] of Word;
const
  CTrans: String = '\VarFileInfo\Translation';
  CProdName: String = '\StringFileInfo\%s\FileDescription';
  CFileVers: String = '\StringFileInfo\%s\FileVersion';
  CCopyright: String = '\StringFileInfo\%s\LegalCopyright';
  CComments: String = '\StringFileInfo\%s\Comments';
  CCompany: String = '\StringFileInfo\%s\CompanyName';
  CSlash: String = '\';
var
  FileName: string;
  iHndl, iSz, iLen: DWORD;
  pBuff: Pointer;
  pTranslation: ^TTranslation;
  sTranslation, sBeta: String;
  pStr: PChar;
  pFileInfo: ^TVSFixedFileInfo;
begin
  Result := Default(TVersionInfo);
  iHndl := 0;
  iLen := 0;
  pFileInfo := nil;
  pTranslation := nil;
  pStr := nil;
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  FileName := AFileName;
  UniqueString(FileName);
  iSz := GetFileVersionInfoSize(PChar(FileName), iHndl);
  if iSz <= 0 then
    Exit;
  GetMem(pBuff, iSz);
  try
    if not GetFileVersionInfo(PChar(AFileName), iHndl, iSz, pBuff) then
      Exit;

    if VerQueryValue(pBuff, PChar(CSlash), Pointer(pFileInfo), iLen) then begin
      Result.FMajorVersion   := pFileInfo^.dwFileVersionMS shr 16;
      Result.FMinorVersion   := pFileInfo^.dwFileVersionMS and $0000FFFF;
      Result.FReleaseVersion := pFileInfo^.dwFileVersionLS shr 16;
      Result.FBuildVersion   := pFileInfo^.dwFileVersionLS and $0000FFFF;

      if (pFileInfo.dwFileFlags and VS_FF_PRERELEASE) <> 0 then
        sBeta := ' Beta';

      Result.FVersionName := Format('%d.%d.%d (Build %d)%s', [Result.FMajorVersion, Result.FMinorVersion, Result.FReleaseVersion, Result.FBuildVersion, sBeta]);
    end;

    if VerQueryValue(pBuff, PChar(CTrans), Pointer(pTranslation), iLen) then
      sTranslation := IntToHex(pTranslation^[0], 4) + IntToHex(pTranslation^[1], 4)
    else
      sTranslation := '040904B0';

    if VerQueryValue(pBuff, PChar(Format(CProdName, [sTranslation])), Pointer(pStr), iLen) then
      Result.FProduct := pStr;

    if VerQueryValue(pBuff, PChar(Format(CFileVers, [sTranslation])), Pointer(pStr), iLen) then
      Result.FVersion := pStr;

    if VerQueryValue(pBuff, PChar(Format(CCopyright, [sTranslation])), Pointer(pStr), iLen) then
      Result.FCopyright := pStr;

    if VerQueryValue(pBuff, PChar(Format(CCompany, [sTranslation])), Pointer(pStr), iLen) then
      Result.FCompany := pStr;

    if VerQueryValue(pBuff, PChar(Format(CComments, [sTranslation])), Pointer(pStr), iLen) then
      Result.FInfo := pStr;

    Result.FIsValid := True;
  finally
    FreeMem(pBuff, iSz);
  end;
end;

var
  FVersionInfo: TVersionInfo;

class function TUtilitiesHelper.GetVersionInfo: TVersionInfo;
begin
  if not FVersionInfo.IsValid then
    FVersionInfo := GetVersionInfo(GetExeName);

  Result := FVersionInfo;
end;

end.
