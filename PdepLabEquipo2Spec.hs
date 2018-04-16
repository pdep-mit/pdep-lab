module Equipo2 where
import PdepLab 
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "equipo 2" $ do
    let cerebro = UnRaton "cerebro" 13 1.3 ["rabia"] "agresivo"
    let micky = UnRaton "micky" 13 1.3 [] "normal"
    let jerry = UnRaton "jerry" 13 1.3 ["tuberculosis","obesidad"] "normal"
    context "iteracion 1 " $ do
      it "al consultar si un raton pesa menos que un valor mayor a su peso retorna True" $ do
          (pesaMenosQue 2 cerebro)  `shouldBe` True
      it "al consultar si un raton pesa menos que un valor menor a su peso retorna False" $ do
	  (pesaMenosQue 0.5 cerebro)  `shouldBe` False
    context "iteracion 2 " $ do
      context "si el raton pesa mas de 2 kilos" $ do
        let cerebro = UnRaton "cerebro" 13 2.1 ["rabia", "vejez"] "normal"
        it "la alcachofa le hace perder el 10% de su peso" $ do   
          (alcachofa  cerebro)  `shouldBe` (cerebro{peso = 2.1 * 0.9} :: Raton)
      context "si el raton pesa menos de 2 kilos" $ do
        let cerebro = UnRaton "cerebro" 13 1.3 ["rabia", "vejez"] "normal"
        it "la alcachofa le hace perder el 5% de su peso" $ do  
          (alcachofa  cerebro)  `shouldBe` (cerebro{peso = 1.3 * 0.95} :: Raton)
    context "iteracion 3 " $ do
      it "al aplicarle un repressitol a un raton sin enfermedades le quita su nombre" $ do
          (repressitol micky)  `shouldBe` (micky{nombre = ""})
      it "al aplicarle un repressitol a un raton con una enfermedad se la quita" $ do
          (repressitol cerebro)  `shouldBe` (cerebro{enfermedades = []} :: Raton)
      it "al aplicarle un repressitol a un raton con N enfermedades le quita la ultima adquirida" $ do
          (enfermedades.repressitol $ jerry)  `shouldSatisfy` (not.elem "tuberculosis")
    context "iteracion 4 " $ do
      it "al aplicar la funcion vamoACalmarno sobre una lista de ratones sin ninguno agresivo los devuelve en el mismo estado" $ do
          (vamoACalmarno [micky] )  `shouldBe` [micky]
      it "al aplicar la funcion vamoACalmarno sobre una lista de ratones con 1 raton agresivo le aplica un rovitril y deja al resto igual" $ do
        (vamoACalmarno [micky,cerebro] )  `shouldBe` [micky,(rovitril cerebro)]
      it "al aplicar la funcion vamoACalmarno sobre una lista vacia de ratones retorna una lista vacia de ratones" $ do
        (vamoACalmarno [] )  `shouldBe` []
    




