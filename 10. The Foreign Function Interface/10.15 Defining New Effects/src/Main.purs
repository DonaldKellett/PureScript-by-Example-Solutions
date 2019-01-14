module Main where

import Effect (Effect, whileE)
import Effect.Console (error)
import Prelude (Unit, bind, discard, (<>), (<$>), const, show)
import Data.Window as Data.Window
import Effect.Alert (alert, confirm, prompt)
import Data.Maybe (Maybe(..), isNothing)
import Effect.Ref (new, modify_, read)
import Data.Window.WebStorage as Data.Window.WebStorage
import Effect.WebStorage (getItem, setItem, removeItem)

main :: Effect Unit
main = do
  window <- Data.Window.window
  alert "Hello there!" window
  maybeName' <- new (Nothing :: Maybe String)
  whileE (isNothing <$> read maybeName') do
    promptName <- prompt "What is your name?" window
    modify_ (const promptName) maybeName'
  maybeName <- read maybeName'
  case maybeName of
    Just name -> do
      alert ("Nice to meet you, " <> name <> "!") window
      runEx2 <- confirm
        ("Would you like to run Exercise 2, " <>
          "an example of Web Storage in PureScript?")
        window
      case runEx2 of
        true -> do
          localStorage <- Data.Window.WebStorage.localStorage window
          sessionStorage <- Data.Window.WebStorage.sessionStorage window
          alert "Testing local and session storage ... " window
          let
            getAnswer :: Data.Window.WebStorage.WebStorage -> Effect (Maybe String)
            getAnswer = getItem "answer"
            setAnswer :: String -> Data.Window.WebStorage.WebStorage -> Effect Unit
            setAnswer = setItem "answer"
            removeAnswer :: Data.Window.WebStorage.WebStorage -> Effect Unit
            removeAnswer = removeItem "answer"
            answer :: Int
            answer = 42
          localAns <- getAnswer localStorage
          sessionAns <- getAnswer sessionStorage
          setAnswer (show answer) localStorage
          localAns' <- getAnswer localStorage
          sessionAns' <- getAnswer sessionStorage
          setAnswer (show answer) sessionStorage
          localAns'' <- getAnswer localStorage
          sessionAns'' <- getAnswer sessionStorage
          removeAnswer localStorage
          localAns''' <- getAnswer localStorage
          sessionAns''' <- getAnswer sessionStorage
          removeAnswer sessionStorage
          localAns'''' <- getAnswer localStorage
          sessionAns'''' <- getAnswer sessionStorage
          alert ("Initial state:\n\nAnswer in local storage: " <>
            show localAns <> "\nAnswer in session storage: " <>
            show sessionAns) window
          alert ("After setting answer in local storage:\n\n" <>
            "Answer in local storage: " <> show localAns' <>
            "\nAnswer in session storage: " <> show sessionAns') window
          alert ("After setting answer in session storage:\n\n" <>
            "Answer in local storage: " <> show localAns'' <>
            "\nAnswer in session storage: " <> show sessionAns'') window
          alert ("After removing answer in local storage:\n\n" <>
            "Answer in local storage: " <> show localAns''' <>
            "\nAnswer in session storage: " <> show sessionAns''') window
          alert ("After removing answer in session storage:\n\n" <>
            "Answer in local storage: " <> show localAns'''' <>
            "\nAnswer in session storage: " <> show sessionAns'''') window
        false -> alert "No problem, see you next time :)" window
    Nothing -> error "Failed to retrieve name of user"
