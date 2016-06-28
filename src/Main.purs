module Main where

import Prelude
import Control.Monad.Eff.Ref
import Graphics.Canvas as C
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM, random, randomInt)
import Data.Array ((..))
import Data.Maybe (Maybe(Just))
import Math (atan2, cos, sin)
import Partial.Unsafe (unsafePartial)

type Particle = {
    x :: Number,
    y :: Number,
    vx :: Number,
    vy :: Number,
    c :: String
}

type ClickData = {
    isDown :: Boolean,
    x :: Number,
    y :: Number
}
clickData :: Boolean -> Number -> Number -> ClickData
clickData d x y = { isDown : d, x, y }

type GameState = {
    particles :: Array Particle,
    leftButton :: ClickData,
    rightButton :: ClickData
}

foreign import requestAnimationFrame :: forall a e. Eff ( canvas :: C.CANVAS | e ) a -> Eff ( canvas :: C.CANVAS | e ) Unit
foreign import onMove :: forall e. Int -> (Boolean -> Number -> Number -> Eff ( canvas :: C.CANVAS | e ) Unit)  -> Eff ( canvas :: C.CANVAS | e ) Unit

foreign import mapE :: forall a b e. Array a -> (a -> Eff e b) -> Eff e (Array b)

initialParticles :: forall e. Eff ( random :: RANDOM | e ) (Array Particle)
initialParticles = mapE (0 .. 500) (\_ -> do
    hue <- randomInt 0 255
    let c = "hsla(" <> (show hue) <> ", 100%, 50%, 0.5)"
    pure { x : 400.0, y : 300.0, vx : 0.0, vy : 0.0, c }
)

initialState :: forall e. Eff ( random :: RANDOM | e ) GameState
initialState = do
    particles <- initialParticles
    pure ({ particles,
            leftButton : clickData false 0.0 0.0,
            rightButton : clickData false 0.0 0.0 })

main :: Eff ( ref :: REF, canvas :: C.CANVAS, random :: RANDOM, console :: CONSOLE ) Unit
main = void $ unsafePartial $ do
    Just canvas <- C.getCanvasElementById "testcanvas"
    C.setCanvasDimensions { width : 800.0, height : 600.0 } canvas

    ctx <- C.getContext2D canvas
    iState <- initialState
    stateRef <- newRef iState

    onMove 0 (\pressed x y -> do
        modifyRef stateRef (_ { leftButton = clickData pressed x y })
    )
    onMove 2 (\pressed x y -> do
        modifyRef stateRef (_ { rightButton = clickData pressed x y })
    )

    let
        update :: forall e. GameState -> Eff ( random :: RANDOM | e ) GameState
        update gs@{ particles, leftButton : lb, rightButton : rb } = do
            let nps1 = if lb.isDown then attractParticles lb.x lb.y particles else particles
                nps2 = if rb.isDown then repelParticles rb.x rb.y particles else nps1

            nps3 <- updateParticles nps2
            pure (gs { particles = nps3 })
            where
                updateParticles :: forall b. Array Particle -> Eff ( random :: RANDOM | b ) (Array Particle)
                updateParticles particles = mapE particles (\p -> do
                        r1 <- random
                        r2 <- random
                        r3 <- random
                        pure (updateParticle [r1, r2, r3] p)
                    )
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
                attractParticles :: Number -> Number -> Array Particle -> Array Particle
                attractParticles x y particles = particles <#> attract x y
                    where
                        attract :: Number -> Number -> Particle -> Particle
                        attract x y p = p { vx = p.vx + dx * strength, vy = p.vy + dy * strength }
                            where
                                strength :: Number
                                strength = 0.5

                                dx :: Number
                                dx = cos (atan2 (y - p.y) (x - p.x))

                                dy :: Number
                                dy = sin (atan2 (y - p.y) (x - p.x))

                repelParticles :: Number -> Number -> Array Particle -> Array Particle
                repelParticles x y particles = particles <#> repel x y
                    where
                        repel :: Number -> Number -> Particle -> Particle
                        repel x y p = p { vx = p.vx - dx * strength, vy = p.vy - dy * strength }
                            where
                                strength :: Number
                                strength = 1.0

                                dx :: Number
                                dx = cos (atan2 (y - p.y) (x - p.x))

                                dy :: Number
                                dy = sin (atan2 (y - p.y) (x - p.x))

        clearCanvas :: forall e. Eff ( canvas :: C.CANVAS | e ) Unit
        clearCanvas = void do
            C.setFillStyle "rgba(0, 0, 0, 0.2)" ctx
            C.fillRect ctx { x : 0.0, y : 0.0, w : 800.0, h : 600.0 }

        drawParticles :: forall e. Array Particle -> Eff ( canvas :: C.CANVAS | e ) Unit
        drawParticles particles = void do
            foreachE particles (\p -> void do
                C.setFillStyle p.c ctx
                C.fillRect ctx { x : p.x, y : p.y, w : 5.0, h : 5.0 }
            )

        loop = void do
            currState <- readRef stateRef
            nextState <- update currState

            clearCanvas
            drawParticles nextState.particles

            writeRef stateRef nextState

            requestAnimationFrame loop

        --loop = (readRef particlesRef >>= update >>= (\nextParticles -> clearCanvas >>= (\_ -> drawParticles nextParticles >>= (\_ -> writeRef particlesRef nextParticles >>= (\_ -> requestAnimationFrame loop >>= (\_ -> pure unit))))))
    loop
