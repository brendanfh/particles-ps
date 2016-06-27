module Main where

import Prelude
import Partial.Unsafe (unsafePartial)

import Data.Array ((..))
import Data.Maybe ( Maybe(Just) )

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Random (RANDOM, random, randomInt)

import Graphics.Canvas as C

type Particle = {
    x :: Number,
    y :: Number,
    vx :: Number,
    vy :: Number,
    c :: String
}

foreign import requestAnimationFrame :: forall a e. Eff ( canvas :: C.CANVAS | e ) a -> Eff ( canvas :: C.CANVAS | e ) Unit

foreign import mapE :: forall a b e. Array a -> (a -> Eff e b) -> Eff e (Array b)

initialParticles :: forall e. Eff ( random :: RANDOM | e ) (Array Particle)
initialParticles = mapE (0 .. 500) (\_ -> do
    hue <- randomInt 0 255
    let c = "hsla(" <> (show hue) <> ", 100%, 50%, 0.5)"
    pure { x : 400.0, y : 300.0, vx : 0.0, vy : 0.0, c }
)

main :: Eff ( ref :: REF, canvas :: C.CANVAS, random :: RANDOM ) Unit
main = void $ unsafePartial $ do
    Just canvas <- C.getCanvasElementById "testcanvas"
    C.setCanvasDimensions { width : 800.0, height : 600.0 } canvas
    
    ctx <- C.getContext2D canvas
    iParticles <- initialParticles
    particlesRef <- newRef iParticles
    
    let
        update :: forall e. Array Particle -> Eff ( random :: RANDOM | e ) (Array Particle)
        update particles = mapE particles (\p -> do
                r1 <- random
                r2 <- random
                r3 <- random
                pure (updateParticle [r1, r2, r3] p)
            )
            where
            updateParticle :: Array Number -> Particle -> Particle
            updateParticle [r1, r2, r3] p =
                let p2 = if r1 > 0.97
                         then p { x = p.x + p.vx,
                                  y = p.y + p.vy,
                                  vx = clamp (-5.0) 5.0 (p.vx + r2 * 1.0 - 0.5),
                                  vy = clamp (-5.0) 5.0 (p.vy + r3 * 1.0 - 0.5) }
                         else p { x = p.x + p.vx,
                                  y = p.y + p.vy }
                    
                    wrapEdges :: { w :: Number, h :: Number } -> Particle -> Particle
                    wrapEdges { w, h } particle = particle { x = wrap 0.0 w particle.x,
                                                             y = wrap 0.0 h particle.y }
                        where
                            wrap :: Number -> Number -> Number -> Number
                            wrap l h v
                                | v < l     = wrap l h (v + h)
                                | v > h     = wrap l h (v - h)
                                | otherwise = v
                        
                in wrapEdges { w : 800.0, h : 600.0 } p2
        
        clearCanvas :: forall e. Eff ( canvas :: C.CANVAS | e ) Unit
        clearCanvas = void do
            C.setFillStyle "black" ctx
            C.fillRect ctx { x : 0.0, y : 0.0, w : 800.0, h : 600.0 }
            
        drawParticles :: forall e. Array Particle -> Eff ( canvas :: C.CANVAS | e ) Unit
        drawParticles particles = void do
            foreachE particles (\p -> void do
                C.setFillStyle p.c ctx
                C.fillRect ctx { x : p.x, y : p.y, w : 10.0, h : 10.0 }
            )
            
        loop = void do
            nextParticles <- (readRef particlesRef >>= update)
            
            clearCanvas
            drawParticles nextParticles
            
            writeRef particlesRef nextParticles
            
            requestAnimationFrame loop
            
        --loop = (readRef particlesRef >>= update >>= (\nextParticles -> clearCanvas >>= (\_ -> drawParticles nextParticles >>= (\_ -> writeRef particlesRef nextParticles >>= (\_ -> requestAnimationFrame loop >>= (\_ -> pure unit))))))
    loop
