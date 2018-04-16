module Equipo4 where
import PdepLab 
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "equipo 4" $ do
    let cerebro = UnRaton "cerebro" 13 1.3 ["tuberculosis","cirrosis"] "normal"
    let micky = UnRaton "micky" 13 6 ["rabia","tuberculosis","obesidad","malaria"] "agresivo"
    context "iteracion 1 " $ do
      it "al consultar si un raton es mas viejo que una edad menor que la suya el resultado es True" $ do
          (esMasViejoQue 1 cerebro)  `shouldBe` True
      it "al consultar si un raton es mas viejo que una edad mayor que la suya el resultado es False" $ do
	  (esMasViejoQue 3 cerebro)  `shouldBe` True
    context "iteracion 2 " $ do
      it "al aplicarle un rovitril a un raton con animo 'normal' lo cambia a 'piola'" $ do
        let cerebro = UnRaton "cerebro" 13 1.3 [] "normal"
        (rovitril cerebro)  `shouldBe` (cerebro{animo = "piola"} :: Raton)
      it "al aplicarle un rovitril a un raton con animo 'agresivo' lo cambia a 'normal'" $ do
        let cerebro = UnRaton "cerebro" 13 1.3 [] "agresivo"
        (rovitril cerebro)  `shouldBe` (cerebro{animo = "normal"} :: Raton)
      it "al aplicarle un rovitril a un raton con animo 'piola' se queda igual" $ do
        let cerebro = UnRaton "cerebro" 13 1.3 [] "piola"
        (rovitril cerebro)  `shouldBe` (cerebro :: Raton)
    context "iteracion 3 " $ do
      it "al aplicarle un curaPlus a un raton hasta las manos le aplica una hierbaZort" $ do
          (curaPlus  micky)  `shouldBe` (hierbaZord micky)
      it "al aplicarle un curaPlus a un raton que no esta hasta las manos le aplica una hierbaVerde de 'sis'" $ do
          (curaPlus cerebro)  `shouldBe` (hierbaVerde "sis" cerebro :: Raton)
    context "iteracion 4 " $ do
      it "el peso promedio en una lista de ratones es el promedio de sus pesos" $ do
          (pesoPromedio [micky,cerebro] )  `shouldBe` (3.65)
      it "el peso promedio en una lista de un raton es su peso" $ do
        (pesoPromedio [micky] )  `shouldBe` 6
      it "el peso promedio en una lista vacia es 0" $ do
        (pesoPromedio [] )  `shouldBe` 0
        
   


    

