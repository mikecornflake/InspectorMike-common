Unit IMMessaging;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, fgl;

Type
  { TIMMessage }

  TIMMessage = Class
  Public
    Sender: TObject;
  End;

  TIMMessageClass = Class Of TIMMessage;

  TIMMessageEvent = Procedure(AMessage: TIMMessage) Of Object;

  { TMessageSubscription }

  TMessageSubscription = Class
    Subscriber: TObject;
    MessageClass: TIMMessageClass;
    Callback: TIMMessageEvent;
  End;

  { TMessageSubscriptions }

  TMessageSubscriptions = Class(Specialize TFPGObjectList<TMessageSubscription>);

  { TMessageBus }

  TMessageBus = Class
  Private
    FStopped: Boolean;
    FSubscriptions: TMessageSubscriptions;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure Stop;

    Procedure Subscribe(ASubscriber: TObject; AMessageClass: TIMMessageClass;
      ACallback: TIMMessageEvent);

    Procedure Unsubscribe(ASubscriber: TObject);

    Procedure Broadcast(AMessage: TIMMessage);
    Procedure Broadcast(ASender: TObject; AMessageClass: TIMMessageClass);
  End;

Implementation

Uses
  LazLogger;

  { TMessageBus }

Constructor TMessageBus.Create;
Begin
  FSubscriptions := TMessageSubscriptions.Create(True);
  FStopped := False;
End;

Destructor TMessageBus.Destroy;
Begin
  FreeAndNil(FSubscriptions);
  Inherited Destroy;
End;

Procedure TMessageBus.Stop;
Begin
  FStopped := True;
End;

Procedure TMessageBus.Subscribe(ASubscriber: TObject; AMessageClass: TIMMessageClass;
  ACallback: TIMMessageEvent);
Var
  oSubscription: TMessageSubscription;
Begin
  oSubscription := TMessageSubscription.Create;
  oSubscription.Subscriber := ASubscriber;
  oSubscription.MessageClass := AMessageClass;
  oSubscription.Callback := ACallback;
  FSubscriptions.Add(oSubscription);
End;

Procedure TMessageBus.Unsubscribe(ASubscriber: TObject);
Var
  i: Integer;
Begin
  For i := FSubscriptions.Count - 1 Downto 0 Do
    If FSubscriptions[i].Subscriber = ASubscriber Then
      FSubscriptions.Delete(i);
End;

Procedure TMessageBus.Broadcast(AMessage: TIMMessage);
Var
  oSubscription: TMessageSubscription;
Begin
  If FStopped Then
    Exit;

  For oSubscription In FSubscriptions Do
    If (AMessage.Sender <> oSubscription.Subscriber) And (AMessage Is
      oSubscription.MessageClass) Then
    Begin
      {$IFNDEF RELEASE}
      DebugLn([ClassName, '.', {$I %CURRENTROUTINE%}, ' Sending ',
        AMessage.ClassName, ' from ', AMessage.Sender.ClassName, ' to ',
        oSubscription.Subscriber.ClassName]);
      {$ENDIF}

      oSubscription.Callback(AMessage);
    End;
End;

Procedure TMessageBus.Broadcast(ASender: TObject; AMessageClass: TIMMessageClass);
Var
  oMessage: TIMMessage;
Begin
  If FStopped Then
    Exit;

  oMessage := AMessageClass.Create;
  Try
    oMessage.Sender := ASender;
    Broadcast(oMessage);
  Finally
    oMessage.Free;
  End;
End;

End.
