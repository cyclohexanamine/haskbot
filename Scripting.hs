{-| module: Scripting

Defines the callbacks that will be called on events. The bot /tries/ to apply
all callbacks to all events, but will ignore any that fail to pattern-match
on their arguments. This means that to define a callback for a certain message,
you just need to write a fuction that only pattern-matches to that kind of message.
Message structures are defined in 'Msg'

For example, the message structure for a PRIVMSG is
@(SPrivMsg Sender Recipient text)@, and more specifically, messages to channels from
users would be @(SPrivmsg (SUser nick user host) (RChannel chan) text@. So if we want
a callback to be called for all channel messages from users, echoing the message
received, we would define a callback function

> echoCallback :: SEvent -> Bot ()
> echoCallback (SPrivmsg (SUser nick _ _) ch@(RChannel _) text) =
>     writeMsg $ CMsg PRIVMSG [show ch, "Echoing: " ++ text]

The Bot () monad allows callbacks to access IO functions (via @liftIO@) and
a stateful store of global variables. Callbacks can simply use keys defined
anywhere; 'Run' doesn't need to know about them, unlike the callbacks.
-}

module Scripting ( callbacks ) where

import Bot ( Bot, SEvent )

import Scripts.Core
import Scripts.Example

-- | The list of callbacks the bot should try to apply.
callbacks :: [SEvent -> Bot ()]
callbacks = [ startup
            , respondToPing
            , respondToChanMsg
            ]



