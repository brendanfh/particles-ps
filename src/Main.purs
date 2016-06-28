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

screenSize :: C.Dimensions
screenSize = { width : 1400.0, height : 750.0 }

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
foreign import onMouseMove :: forall e. Int -> (Boolean -> Number -> Number -> Eff ( canvas :: C.CANVAS | e ) Unit) -> Eff ( canvas :: C.CANVAS | e ) Unit

foreign import mapE :: forall a b e. Array a -> (a -> Eff e b) -> Eff e (Array b)

initialParticles :: forall e. Eff ( random :: RANDOM | e ) (Array Particle)
initialParticles = mapE (0 .. 500) (\_ -> do
    hue <- randomInt 0 255
    let c = "hsla(" <> (show hue) <> ", 100%, 50%, 1.0)"
    pure { x : screenSize.width / 2.0, y : screenSize.height / 2.0, vx : 0.0, vy : 0.0, c }
)

initialState :: forall e. Eff ( random :: RANDOM | e ) GameState
initialState = do
    particles <- initialParticles
    pure ({ particles,
            leftButton : clickData false 0.0 0.0,
            rightButton : clickData false 0.0 0.0 })

update :: forall e. GameState -> Eff ( random :: RANDOM | e ) GameState
update gs@{ particles, leftButton : lb, rightButton : rb } = do
    particles' <- (mouseCheck lb attractParticles >>> mouseCheck rb repelParticles >>> updateParticles) particles
    pure (gs { particles = particles' })
    where
        mouseCheck :: ClickData -> (Number -> Number -> Array Particle -> Array Particle) -> Array Particle -> Array Particle
        mouseCheck { isDown, x, y } cb particles = if isDown then cb x y particles else particles

        updateParticles :: forall b. Array Particle -> Eff ( random :: RANDOM | b ) (Array Particle)
        updateParticles particles = mapE particles (\p -> do
            --shouldChange <- random
            --ifM (random <#> ((flip (>)) 0.97)) (do
            --ifM (pure (shouldChange > 0.97)) (do
            ifM (cmp random (>) 0.97) (do
                    rcx <- random
                    rcy <- random
                    pure (updateParticle (scaleR rcx) (scaleR rcy) p)
                ) (do
                    pure (updateParticle 0.0 0.0 p)
                )
            )
            where
                scaleR :: Number -> Number
                scaleR a = a * 10.0 - 5.0

                cmp :: forall a f. (Ord a, Functor f) => f a -> (a -> a -> Boolean) -> a -> f Boolean
                cmp a c b = a <#> ((flip c) b)

        updateParticle :: Number -> Number -> Particle -> Particle
        updateParticle rx ry p =
            let p2 = p { x = p.x + p.vx,
                         y = p.y + p.vy,
                         vx = p.vx * 0.97 + rx,
                         vy = p.vy * 0.97 + ry }

                clampVel :: Particle -> Particle
                clampVel p = p { vx = clamp (-maxSpd) maxSpd p.vx,
                                 vy = clamp (-maxSpd) maxSpd p.vy }
                    where
                        maxSpd :: Number
                        maxSpd = 10.0

                wrapEdges :: C.Dimensions -> Particle -> Particle
                wrapEdges { width : w, height : h } particle = particle { x = wrap 0.0 w particle.x,
                                                                          y = wrap 0.0 h particle.y }
                    where
                        wrap :: Number -> Number -> Number -> Number
                        wrap l h v
                            | v < l     = wrap l h (v + h)
                            | v > h     = wrap l h (v - h)
                            | otherwise = v

            in (wrapEdges screenSize >>> clampVel) p2

        attractParticles :: Number -> Number -> Array Particle -> Array Particle
        attractParticles x y particles = particles <#> attract x y
            where
                attract :: Number -> Number -> Particle -> Particle
                attract x y p = p { vx = p.vx + dx * strength, vy = p.vy + dy * strength }
                    where
                        strength :: Number
                        strength = 1.0

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

clearCanvas :: forall e. C.Context2D -> Eff ( canvas :: C.CANVAS | e ) Unit
clearCanvas ctx = void do
    C.setFillStyle "rgba(0, 0, 0, 0.25)" ctx
    C.fillRect ctx { x : 0.0, y : 0.0, w : screenSize.width, h : screenSize.height }

drawParticles :: forall e. Array Particle -> C.Context2D -> Eff ( canvas :: C.CANVAS | e ) Unit
drawParticles particles ctx = void do
    foreachE particles (\p -> void do
        C.setFillStyle p.c ctx
        C.fillRect ctx { x : p.x, y : p.y, w : 5.0, h : 5.0 }
    )

main :: Eff ( ref :: REF, canvas :: C.CANVAS, random :: RANDOM, console :: CONSOLE ) Unit
main = void $ unsafePartial $ do
    Just canvas <- C.getCanvasElementById "testcanvas"
    C.setCanvasDimensions screenSize canvas
    ctx <- C.getContext2D canvas

    iState <- initialState
    stateRef <- newRef iState

    onMouseMove 0 (\pressed x y -> do
        modifyRef stateRef (_ { leftButton = clickData pressed x y })
    )
    onMouseMove 2 (\pressed x y -> do
        modifyRef stateRef (_ { rightButton = clickData pressed x y })
    )

    let loop = void do
            currState <- readRef stateRef
            nextState <- update currState

            clearCanvas ctx
            drawParticles nextState.particles ctx

            writeRef stateRef nextState

            requestAnimationFrame loop
    requestAnimationFrame loop
