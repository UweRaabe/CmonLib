unit Cmon.Factory;

interface

uses
  System.SysUtils, System.Generics.Collections;

type
  TFactory<T> = class
  type
    TCreator = TFunc<T>;
    TCreatorRec = record
      FCreator: TCreator;
      FTypeInfo: Pointer;
    end;
  strict private
  class var
    FCreatorRec: TCreatorRec;
  strict protected
    class procedure HandleNoImplementor; virtual;
    class function MakeCreatorRec<TImpl: constructor, T>: TCreatorRec;
  public
    class function CreateInstance: T; overload;
    class procedure Register<TImpl: constructor, T>; overload;
    class procedure Unregister<TImpl: constructor, T>; overload;
  end;

  TFactory<T; TContext> = class(TFactory<T>)
  type
    TCreators = TDictionary<TContext, TFactory<T>.TCreatorRec>;
  strict private
  class var
    FCreators: TCreators;
    class destructor ClassDestroy;
    class function GetCreators: TCreators; static;
  strict protected
    class function UseDefaultOnNoContext: Boolean; virtual;
    class property Creators: TCreators read GetCreators;
  public
    class function CreateInstance(const Key: TContext): T; overload; virtual;
    class procedure Register<TImpl: constructor, T>(const Key: TContext);
    class procedure Unregister<TImpl: constructor, T>(const Key: TContext);
  end;

type
  TClassFactory<T: class> = class(TFactory<T>);
  TClassFactory<T: class; TContext> = class(TFactory<T,TContext>);

type
{ Besides real interfaces the IInterface constraint also allows to pass any class type supporting the given interface.
  This must be avoided (Delphi 13 introduces such an interface constraint) and is therefore enforced by assertions at runtime. }

{$IF CompilerVersion < 37.0 Delphi 13 Florence }
  TInterfaceFactory<I:IInterface> = class(TFactory<I>)
  strict private
    class constructor CreateClass;
  public
  end;

  TInterfaceFactory<I:IInterface; TContext> = class(TFactory<I, TContext>)
  strict private
    class constructor CreateClass;
  public
  end;
{$ELSE}
  TInterfaceFactory<I:interface> = class(TFactory<I>);
  TInterfaceFactory<I:interface; TContext> = class(TFactory<I, TContext>);
{$ENDIF}

implementation

uses
  System.TypInfo;

type
  Check = record
  public
    class procedure ForInterface<T>; static; inline;
  end;

class procedure Check.ForInterface<T>;
begin
{$IFDEF _}
  Assert(PTypeInfo(TypeInfo(T)).Kind = tkInterface, 'only interface types are allowed');
{$ENDIF}
end;

class function TFactory<T>.CreateInstance: T;
begin
  Result := Default(T);
  if Assigned(FCreatorRec.FCreator) then
    Result := FCreatorRec.FCreator()
  else
    HandleNoImplementor;
end;

class procedure TFactory<T>.HandleNoImplementor;
begin
  raise ENotImplemented.Create('no factory method registered');
end;

class function TFactory<T>.MakeCreatorRec<TImpl>: TCreatorRec;
begin
  Result.FTypeInfo := TypeInfo(TImpl);
  Result.FCreator := function: T
    begin
      Result := TImpl.Create;
    end;
end;

class procedure TFactory<T>.Register<TImpl>;
begin
  FCreatorRec := MakeCreatorRec<TImpl>;
end;

class procedure TFactory<T>.Unregister<TImpl>;
begin
  if FCreatorRec.FTypeInfo = TypeInfo(T) then begin
    FCreatorRec.FCreator := nil;
    FCreatorRec.FTypeInfo := nil;
  end;
end;

class destructor TFactory<T, TContext>.ClassDestroy;
begin
  FCreators.Free;
end;

class function TFactory<T, TContext>.CreateInstance(const Key: TContext): T;
var
  rec: TCreatorRec;
begin
  Result := Default(T);
  if Creators.TryGetValue(Key, rec) then
    Result := rec.FCreator()
  else if UseDefaultOnNoContext then
    Result := CreateInstance
  else
    HandleNoImplementor;
end;

class function TFactory<T, TContext>.GetCreators: TCreators;
begin
  if FCreators = nil then begin
    FCreators := TCreators.Create;
  end;
  Result := FCreators;
end;

class procedure TFactory<T, TContext>.Register<TImpl>(const Key: TContext);
begin
  Creators.AddOrSetValue(Key, MakeCreatorRec<TImpl>);
end;

class procedure TFactory<T, TContext>.Unregister<TImpl>(const Key: TContext);
var
  rec: TCreatorRec;
begin
  if Creators.TryGetValue(Key, rec) and (rec.FTypeInfo = TypeInfo(T)) then
    Creators.Remove(key);
end;

class function TFactory<T, TContext>.UseDefaultOnNoContext: Boolean;
begin
  Result := False;
end;

{$IF CompilerVersion < 37.0 Delphi 13 Florence }
class constructor TInterfaceFactory<I>.CreateClass;
begin
  Check.ForInterface<I>;
end;

class constructor TInterfaceFactory<I, TContext>.CreateClass;
begin
  Check.ForInterface<I>;
end;
{$ENDIF}

end.
