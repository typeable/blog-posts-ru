module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List.Extra exposing (updateAt)
import List
import Tuple
import Html.Attributes exposing (style, class)
import String

-- * Типы данных, описывающие состояние виджета.

type alias QuestionText = String

type alias AnswerText = String

type IsChosen = Chosen | NotChosen

type IsCorrect = Correct | Incorrect

type CanCheckAnswers = CanCheckAnswers | CantCheckAnswers

type AreAnswersShown = AnswersShown | AnswersHidden

type alias Answer =
    { answerText : AnswerText
    , isCorrect : IsCorrect
    }

type Score
    = NoScore
    | Score { totalQuestions : Int, correctAnswers : Int }

type alias Questions = List (QuestionText, List (Answer, IsChosen))

type alias Model
    = { areAnswersShown : AreAnswersShown
      , allQuestions : Questions
      , canCheckAnswers : CanCheckAnswers
      , score : Score
      }

-- ** Тип данных, описывающий payload события (Message).

type Msg
    = SelectAnswer
      { questionNumber : Int
      , answerNumber : Int
      , isChosen : IsChosen }
    | CheckAnswers

update : Msg -> Model -> Model
update msg = case msg of
  SelectAnswer { questionNumber, answerNumber, isChosen } ->
      updateCanCheckAnswers <<
      ( mapAllQuestions
        <| updateAt questionNumber
        <| Tuple.mapSecond
        <| updateAnswers answerNumber isChosen )
  CheckAnswers -> mapAnswersShown (\_ -> AnswersShown) >> updateScore

updateCanCheckAnswers : Model -> Model
updateCanCheckAnswers model =
    { model | canCheckAnswers =
          if List.all hasChosenAnswer model.allQuestions
          then CanCheckAnswers
          else CantCheckAnswers }

updateScore : Model -> Model
updateScore model =
    let
        hasCorrectAnswer (_, answers) =
            List.any isCorrectAnswerChosen answers
        correctAnswers =
            List.length <| List.filter hasCorrectAnswer model.allQuestions
        totalQuestions = List.length model.allQuestions
    in
        { model | score =
              Score { correctAnswers = correctAnswers
                    , totalQuestions = totalQuestions } }

updateAnswers : Int -> IsChosen -> List (Answer, IsChosen) -> List (Answer, IsChosen)
updateAnswers answerIx newIsChosen =
    List.indexedMap <| \aix ->
        Tuple.mapSecond <| \isChosen ->
            if aix /= answerIx
            then
                if newIsChosen == Chosen
                then NotChosen
                else isChosen
            else newIsChosen

view : Model -> Html Msg
view model =
    div [] <|
      List.indexedMap (viewQuestion model.areAnswersShown) model.allQuestions ++
      [ div [ class "check-answers-button-container" ] [ viewFooter model ] ]

viewQuestion : AreAnswersShown -> Int -> (QuestionText, List (Answer, IsChosen)) -> Html Msg
viewQuestion areShown questionIx (question, answers) =
    div [class "question"] <|
        [ text question ] ++
        [ div [class "answers"]
          <| List.indexedMap (viewAnswer areShown questionIx) answers ]

viewAnswer : AreAnswersShown -> Int -> Int -> (Answer, IsChosen) -> Html Msg
viewAnswer areShown questionIx answerIx (answer, isChosen) =
    let
        events = [ onClick <|
                       SelectAnswer { questionNumber = questionIx
                                    , answerNumber = answerIx
                                    , isChosen = toggleChosen isChosen
                                    } ]
        className = String.join " " <|
            ["answer"] ++
            ( if isChosen == Chosen
              then ["answer-chosen"]
              else [] ) ++
            ( if areShown == AnswersShown
              then ["answer-shown"]
              else ["answer-hidden"] ) ++
            ( if answer.isCorrect == Correct
              then ["answer-correct"]
              else ["answer-incorrect"] )
        attrs = [ class className ]
    in
        div (attrs ++ events) [ text answer.answerText ]

viewFooter : Model -> Html Msg
viewFooter model =
    case model.score of
        NoScore ->
            case model.canCheckAnswers of
                CanCheckAnswers ->
                    button [ onClick CheckAnswers ] [ text "Check answers" ]
                CantCheckAnswers ->
                    div [ class "unfinished-quiz-notice" ]
                        [ text "Select answers for all questions before you can get the results." ]
        Score { totalQuestions, correctAnswers } ->
            text <|
                "Your score: " ++ String.fromInt correctAnswers ++
                " of " ++ String.fromInt totalQuestions

-- * Вспомогательные функции

toggleChosen : IsChosen -> IsChosen
toggleChosen isChosen =
    case isChosen of
        Chosen -> NotChosen
        NotChosen -> Chosen

hasChosenAnswer : (QuestionText, List (Answer, IsChosen)) -> Bool
hasChosenAnswer (_, answers) =
    List.any (\(_, isChosen) -> isChosen == Chosen) answers

isCorrectAnswerChosen : (Answer, IsChosen) -> Bool
isCorrectAnswerChosen (answer, isChosen) =
    case (answer.isCorrect, isChosen) of
        (Correct, Chosen) -> True
        _ -> False

mapAllQuestions : (Questions -> Questions) -> Model -> Model
mapAllQuestions f model = { model | allQuestions = f model.allQuestions }

mapAnswersShown : (AreAnswersShown -> AreAnswersShown) -> Model -> Model
mapAnswersShown f model = { model | areAnswersShown = f model.areAnswersShown }

mapCanCheckAnswers : (CanCheckAnswers -> CanCheckAnswers) -> Model -> Model
mapCanCheckAnswers f model = { model | canCheckAnswers = f model.canCheckAnswers }

allQuestions : Questions
allQuestions =
    [ ( "How many arguments does update function have?"
      , [ ({ answerText = "1", isCorrect = Incorrect }, NotChosen)
        , ({ answerText = "2", isCorrect = Correct }, NotChosen)
        , ({ answerText = "3", isCorrect = Incorrect }, NotChosen)
        , ({ answerText = "4", isCorrect = Incorrect }, NotChosen)
        ] )
    , ( "Which of the following does not contain a value at any point of time?"
      , [ ({ answerText = "Behavior", isCorrect = Incorrect }, NotChosen)
        , ({ answerText = "Event", isCorrect = Correct }, NotChosen)
        , ({ answerText = "Dynamic", isCorrect = Incorrect }, NotChosen)
        ] )
    , ( "Which framework does not use Virtual DOM?"
      , [ ({ answerText = "Elm", isCorrect = Incorrect }, NotChosen)
        , ({ answerText = "Reflex", isCorrect = Correct }, NotChosen)
        ] )
    ]

initialModel =
  { areAnswersShown = AnswersHidden
  , allQuestions = allQuestions
  , score = NoScore
  , canCheckAnswers = CantCheckAnswers
  }

main =
  Browser.sandbox { init = initialModel, update = update, view = view }
