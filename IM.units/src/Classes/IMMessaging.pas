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

  TIMMessageEvent = procedure(AMessage: TIMMessage) of object;

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
    FSubscriptions: TMessageSubscriptions;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure Subscribe(ASubscriber: TObject; AMessageClass: TIMMessageClass;
      ACallback: TIMMessageEvent);

    Procedure Broadcast(AMessage: TIMMessage);
  End;

Implementation

Uses
  LazLogger;

{ TMessageBus }

Constructor TMessageBus.Create;
Begin
  FSubscriptions := TMessageSubscriptions.Create(True);
End;

Destructor TMessageBus.Destroy;
Begin
  FreeAndNil(FSubscriptions);
  Inherited Destroy;
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

Procedure TMessageBus.Broadcast(AMessage: TIMMessage);
Var
  oSubscription: TMessageSubscription;
Begin
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

End.
