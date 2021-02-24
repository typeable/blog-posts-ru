module Frontend where

import           Common.Route
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Foldable
import           Data.Functor
import qualified Data.Text                as T
import           Data.Traversable
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Reflex.Dom.Core

-- * Типы данных, описывающие состояние виджета.

type QuestionText = T.Text

type AnswerText = T.Text

data IsChosen = Chosen | NotChosen
  deriving (Show, Eq)

data IsCorrect = Correct | Incorrect
  deriving (Show, Eq)

data CanCheckAnswers = CanCheckAnswers | CantCheckAnswers
  deriving (Show, Eq)

data AreAnswersShown = AnswersShown | AnswersHidden
  deriving (Show, Eq)

data Answer = Answer
  { answerText :: AnswerText
  , isCorrect  :: IsCorrect }
  deriving (Show, Eq)

data Score
  = NoScore
  | Score
    { totalQuestions :: Int
    , correctAnswers :: Int }
  deriving (Eq, Show)

-- ** Типы данных, описывающие payload различных событий.

-- | Выбор варианта ответа.
data SelectAnswer
  = SelectAnswer
  { questionNumber :: Int
  , answerNumber   :: Int }

-- | Payload для события "показать ответы"
data CheckAnswers = CheckAnswers

-- ** Типы данных, описывающие внутренние интерфейсы нашего виджета.

-- | Тип, содержащий все события, с которыми мы будем работать.
data QuizEvents t = QuizEvents
  { selectAnswer :: Event t SelectAnswer
  , showAnswers  :: Event t CheckAnswers }

-- | Тип, содержащий все изменяющиеся (Dynamic) данные.
data QuizState t = QuizState
  { areAnswersShown :: Dynamic t AreAnswersShown
  , allQuestions    :: [(QuestionText, [(Answer, Dynamic t IsChosen)])]
  , canCheckAnswers :: Dynamic t CanCheckAnswers
  , score           :: Dynamic t Score }

-- * Model

-- | Функция, описывающая всю внутреннюю логику работы виджета. Она принимает
-- тип данных 'QuizEvents', хранящий все события и возвращает динамическое состояние.
mkQuizModel :: ObeliskWidget js t route m
  => [(QuestionText, [Answer])]
  -- ^ Список вопросов с ответами
  -> QuizEvents t
  -> m (QuizState t)
mkQuizModel questions evs = do
  areAnswersShown <- holdDyn AnswersHidden $ showAnswers evs $> AnswersShown
  allQuestions <- do
    for (enumerate questions) \(qNum, (questionText, answers)) -> do
      (questionText, ) <$> for (enumerate answers) \(aNum, Answer{..}) -> do
        let
          updChosenState SelectAnswer{questionNumber,answerNumber} isChosen = do
            guard (questionNumber == qNum)
            return
              if answerNumber == aNum
              then toggleChosen isChosen
              else NotChosen
        isChosenDyn <- foldDynMaybe updChosenState NotChosen (selectAnswer evs)
        return (Answer{..}, isChosenDyn)
  let
    canCheckAnswers = do
      -- Для каждого вопроса хотя бы один ответ выбран
      status <- all (Chosen `elem`) <$> do
        for allQuestions \(_, answers) -> do
          for answers \(_, dynIsChosen) -> dynIsChosen
      return if status then CanCheckAnswers else CantCheckAnswers
    score = do
      areAnswersShown >>= \case
        AnswersHidden -> return NoScore
        AnswersShown -> do
          correctAnswers <- flip execStateT 0 do
            for_ allQuestions \(_, answers) -> do
              for_ answers \(answer, dynIsChosen) -> do
                isChosen <- lift dynIsChosen
                when (isChosen == Chosen && isCorrect answer == Correct) do
                  modify (+ 1)
          return Score {correctAnswers, totalQuestions = length allQuestions}
  return QuizState{..}

-- * Интерфейс пользователя

quizUI :: ObeliskWidget js t route m => QuizState t -> m (QuizEvents t)
quizUI QuizState{..} = wrapUI do
  selectAnswer <- leftmost <$> for (enumerate allQuestions)
    \(qNum, (questionText, answers)) -> do
      divClass "question" do
        text questionText
      answersUI qNum areAnswersShown answers
  showAnswers <- footerUI canCheckAnswers score
  return QuizEvents{..}

answersUI :: ObeliskWidget js t route m
  => Int
  -> Dynamic t AreAnswersShown
  -> [(Answer, Dynamic t IsChosen)]
  -> m (Event t SelectAnswer)
answersUI qNum areAnswersShown answers = do
  leftmost <$> elClass "div" "answers" do
    for (enumerate answers)
      \(aNum, (Answer{answerText,isCorrect}, dynIsChosen)) -> do
        let
          dynAttrs = do
            isChosen <- dynIsChosen
            areShown <- areAnswersShown
            let
              className = T.intercalate " " $ execWriter do
                tell ["answer"]
                when (isChosen == Chosen) $ tell ["answer-chosen"]
                tell [ if areShown == AnswersShown
                       then "answer-shown"
                       else "answer-hidden" ]
                tell [ if isCorrect == Correct
                       then "answer-correct"
                       else "answer-incorrect" ]
            return $ "class" =: className
        event <- domEvent Click . fst <$> elDynAttr' "div" dynAttrs do
          text answerText
        return $ event $>
          SelectAnswer { questionNumber = qNum, answerNumber = aNum }

-- | Виджет, содержащий либо текст с предложением ответить на все вопросы, либо
-- кнопку проверки ответов, либо информацию о результатах.
footerUI :: ObeliskWidget js t route m
  => Dynamic t CanCheckAnswers -> Dynamic t Score -> m (Event t CheckAnswers)
footerUI canCheckAnswersDyn dynScore = wrapContainer do
  evt <- switchHold never <=< dyn $ do
    canCheckAnswers <- canCheckAnswersDyn
    score <- dynScore
    return if score /= NoScore
      then return never
      else canCheckAnswers & \case
      CanCheckAnswers -> do
        domEvent Click . fst <$> do
          el' "button" do
            text "Check answers"
      CantCheckAnswers -> do
        divClass "unfinished-quiz-notice" do
          text "Select answers for all questions before you can get the results."
        return never
  dyn_ $ dynScore <&> \case
    NoScore -> blank
    Score{totalQuestions, correctAnswers} -> do
      text "Your score: "
      text . T.pack $ show correctAnswers
      text " of "
      text . T.pack $ show totalQuestions
  return $ evt $> CheckAnswers
  where wrapContainer = divClass "check-answers-button-container"

wrapUI :: ObeliskWidget js t route m => m a -> m a
wrapUI contents = do
  el "h1" do
    text "Quiz Demo using "
    mkLink "Reflex framework" "https://reflex-frp.org/"
  result <- elAttr "div" ("id" =: "container") contents
  el "h3" do
    text "Demo made by "
    mkLink "Typeable" "https://typeable.io/"
  return result
  where
    mkLink linkText url = do
      elAttr "a" ("href" =: url <> "target" =: "blank") do
        text linkText

-- * Main widget

questions :: [(QuestionText, [Answer])]
questions =
  [ ( "How many arguments does update function have?"
    , [ Answer "1" Incorrect
      , Answer "2" Correct
      , Answer "3" Incorrect
      , Answer "4" Incorrect
      ] )
  , ( "Which of the following does not contain a value at any point of time?"
    , [ Answer "Behaviour" Incorrect
      , Answer "Event" Correct
      , Answer "Dynamic" Incorrect
      ] )
  , ( "Which framework does not use Virtual DOM?"
    , [ Answer "Elm" Incorrect
      , Answer "Reflex" Correct
      ] )
  ]

mkQuizWidget :: ObeliskWidget js t route m => [(QuestionText, [Answer])] -> m ()
mkQuizWidget qs =
  mkWidget (mkQuizModel qs) quizUI

-- * Вспомогательные функции.

mkWidget :: ObeliskWidget js t route m
  => (events -> m state) -> (state -> m events) -> m ()
mkWidget model ui  = void (mfix (model >=> ui))

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

toggleChosen :: IsChosen -> IsChosen
toggleChosen Chosen    = NotChosen
toggleChosen NotChosen = Chosen

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Quiz Demo (Reflex)"
      elAttr "link"
        ( "href" =: static @"main.css" <>
          "type" =: "text/css" <>
          "rel" =: "stylesheet" ) blank
  , _frontend_body = do
      el "div" do
        mkQuizWidget questions
      return ()
  }
