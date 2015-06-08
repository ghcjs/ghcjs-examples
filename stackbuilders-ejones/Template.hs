module Template (nameForm) where

import           Text.Blaze.Html5 hiding (select, html, em)
import qualified Text.Blaze.Html5.Attributes as A

nameForm :: Html
nameForm = section ! A.id "formContainer" $
  form $ do
    labeledInputField "firstNameInput" "First Name" False
    labeledInputField "lastNameInput"  "Last Name" False
    labeledInputField "fullNameInput"  "Full Name" True

labeledInputField :: AttributeValue -> Html -> Bool -> Html
labeledInputField id_ label_ readonly = p $ do
  input ! A.type_ "text"
        ! A.maxlength "10"
        ! A.id id_
        !? (readonly, A.readonly "")
  label label_
