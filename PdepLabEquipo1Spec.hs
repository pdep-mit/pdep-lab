module Equipo1 where
import PdepLab 
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "equipo 1" $ do
    let cerebro = UnRaton "cerebro" 1 1 ["rabia"] "normal"
    let pinky = UnRaton "pinky" 1 1 ["rabia", "tuberculosis"] "normal"
    context "iteracion 1 " $ do
      it "al consultar si un raton esta enfermo de una enfermedad que posee el resultado es True" $ do
          (estaEnfermoDe "rabia" cerebro)  `shouldBe` True
      it "al consultar si un raton esta enfermo de una enfermedad que no posee el resultado es False" $ do
	  (estaEnfermoDe "sarampion" cerebro)  `shouldBe` False
    context "iteracion 2 " $ do
      it "al aplicarle una hierbaVerde de vejez a un raton se reduce su edad a la mitad" $ do
	(hierbaVerde "vejez" cerebro)  `shouldBe` (cerebro{edad = 0.5} :: Raton)
      it "al aplicarle una hierbaVerde de 'bia' a un raton con rabia se cura de rabia" $ do
	(hierbaVerde "bia" cerebro)  `shouldBe` (cerebro{enfermedades = ["vejez"]} :: Raton)
    context "iteracion 3 " $ do
      it "al aplicarle un pontsAntiAge a un raton se le aplica consecutivamente 3 hierbasVerdes de vejez y finalmente una alcachofa" $ do
        (pondsAntiAge cerebro)  `shouldBe` (cerebro{peso = 0.95,edad = 0.125 } :: Raton)
    context "iteracion 4 " $ do
      it "Una enfermedad es epidemia si todos los ratones la poseen" $ do
        (epidemia "rabia" [cerebro,pinky])  `shouldBe` True
      it "Una enfermedad no es epidemia si almenos 1 raton no la posee" $ do
        (epidemia "tuberculosis" [cerebro,pinky])  `shouldBe` False
      


