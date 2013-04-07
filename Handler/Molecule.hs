{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Molecule
  ( getMoleculeR
  , postMoleculeR
  ) where

import Import
import Data.Conduit(($$))
import Data.Conduit.List(consume)
import Data.Text.Encoding(decodeUtf8)
import Ouch.Input.Smiles(readSmi)
import Ouch.Output.Smiles(writeSmiles)
import Ouch.Property.Composition(atomCount, molecularFormula, molecularWeight)
import Ouch.Structure.Molecule(Molecule)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Ouch.Property.Builder as OPB
import Text.Printf(printf)

getMoleculeR :: Handler RepHtml
getMoleculeR = do
    (formWidget, formEnctype) <- generateFormPost importForm
    let moleculesAndFilename = Nothing :: Maybe ([Molecule], Text)
    defaultLayout $ do
        $(widgetFile "molecule")

postMoleculeR :: Handler RepHtml
postMoleculeR = do
    ((result, formWidget), formEnctype) <- runFormPost importForm
    moleculesAndFilename <- processFile result
    defaultLayout $ do
        $(widgetFile "molecule")

importForm :: Form FileInfo
importForm = renderDivs $ fileAFormReq "Choose a SMILES file:"

processFile :: FormResult FileInfo -> Handler (Maybe ([Molecule], Text))
processFile formRes =
    case formRes of
      FormSuccess fi -> do
        molecules <- lift $ toMolecules <$> (fileSource fi $$ consume)
        let fName = fileName fi
        return . Just $ (molecules, fName)
      _ -> return Nothing
  where
    smiles :: [B.ByteString] -> [Text]
    smiles = T.lines . decodeUtf8 . B.concat
    toMolecules = map (readSmi . T.unpack) . smiles

showMolForm :: Molecule -> String
showMolForm = showProperty molecularFormula

showAtomCount :: Molecule -> String
showAtomCount = showProperty atomCount

showMolWeight :: Molecule -> String
showMolWeight mol = let (OPB.DoubleValue d) = getPropertyValue molecularWeight mol
                     in printf "%.2f\n" d

getPropertyValue :: OPB.Property -> Molecule -> OPB.Value
getPropertyValue prop m = case OPB.value prop of
                            Left  v -> v
                            Right f -> f m

showProperty :: OPB.Property -> Molecule -> String
showProperty p m = show $ getPropertyValue p m

