module Equipo3 where
import PdepLab 
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "equipo 3" $ do
    let cerebro = UnRaton "cerebro" 13 1.3 ["rabia"] "normal"
    let jerry = UnRaton "jerry" 13 1 ["rabia", "tuberculosis", "obesidad"] "piola"
    let micky = UnRaton "micky" 13 6 ["rabia","tuberculosis","obesidad","malaria"] "agresivo"
    context "iteracion 1 " $ do
      it "al consultar si un raton con mas de 3 enfermedades esta hasta las manos el resultado es False" $ do
          (hastaLasManos cerebro)  `shouldBe` False
      it "al consultar si un raton con menos de 3 enfermedades el resultado es True" $ do
	  (hastaLasManos micky)  `shouldBe` True
      it "al consultar si un raton con 3 enfermedades el resultado es False" $ do
	  (hastaLasManos jerry)  `shouldBe` False
    context "iteracion 2 " $ do
      let pinky = UnRaton "pinky" 0 0 [] "piola"
      it "al aplicarle una hierbaZord a un raton lo transforma en pinky" $ do
          (hierbaZord  cerebro)  `shouldBe` (pinky{peso = peso cerebro} :: Raton)
    context "iteracion 3 " $ do
      it "al aplicarle un reduceFatFast a un raton con obesidad la pierde" $ do
          (enfermedades.reduceFatFast $ jerry)  `shouldSatisfy` (not.elem "obesidad")
      it "al aplicarle un reduceFatFast a un raton se le aplica consecutivamente 2 alcachofas" $ do
        (reduceFatFast cerebro)  `shouldBe` (cerebro{peso = 1.17325} :: Raton)
    context "iteracion 4 " $ do
      it "si la lista contiene solo ratones que al aplicarle la alcachofa quedan por debajo del kg estanMasLindosQueNunca retorna True" $ do
          (estanMasLindosQueNunca [jerry])  `shouldBe` True
      it "Si la lista contiene al menos un raton cuyo peso no queda por debajo del kg al aplicarle la alcachofa estanMasLindosQueNunca retorna false " $ do
        (estanMasLindosQueNunca [micky,jerry])  `shouldBe` False
      it "si la lista contiene un raton cuyo peso no queda por debajo del kg al aplicarle la alcahcofa estanMasLindosQueNunca retorna false" $ do
        (estanMasLindosQueNunca [cerebro])  `shouldBe` True
      



