--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Axis2d exposing
    ( Axis2d
    , x, y
    , through, withDirection, throughPoints
    , originPoint, direction
    , intersectionPoint, intersectionWithCircle
    , reverse, moveTo, rotateAround, rotateBy, translateBy, translateIn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    )

{-| An `Axis2d` represents an infinitely long straight line in 2D and is defined
by an origin point and direction. Axes have several uses, such as:

  - Mirroring across the axis
  - Projecting onto the axis
  - Measuring distance along the axis from the origin point

@docs Axis2d


# Constants

@docs x, y


# Constructors

@docs through, withDirection, throughPoints


# Properties

@docs originPoint, direction


# Intersection

@docs intersectionPoint, intersectionWithCircle


# Transformations

@docs reverse, moveTo, rotateAround, rotateBy, translateBy, translateIn, mirrorAcross


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Angle exposing (Angle)
import Direction2d exposing (Direction2d)
import Geometry.Types as Types exposing (Circle2d, Frame2d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..), Rate)
import Vector2d exposing (Vector2d)


{-| -}
type alias Axis2d units coordinates =
    Types.Axis2d units coordinates


{-| The global X axis.

    Axis2d.x
    --> Axis2d.through Point2d.origin Direction2d.x

-}
x : Axis2d units coordinates
x =
    through Point2d.origin Direction2d.x


{-| The global Y axis.

    Axis2d.y
    --> Axis2d.through Point2d.origin Direction2d.y

-}
y : Axis2d units coordinates
y =
    through Point2d.origin Direction2d.y


{-| Construct an axis through the given origin point with the given direction.

    exampleAxis =
        Axis2d.through (Point2d.meters 1 3)
            (Direction2d.degrees 30)

-}
through : Point2d units coordinates -> Direction2d coordinates -> Axis2d units coordinates
through givenPoint givenDirection =
    Types.Axis2d { originPoint = givenPoint, direction = givenDirection }


{-| Construct an axis with the given direction, through the given origin point.
Flipped version of `through`. Having both versions allow you to do different
things with partial application:

    -- A list of axes in different directions all passing
    -- through the same origin point
    List.map (Axis2d.through point) directions

    -- A list of parallel axes (all having the same
    -- direction) through different points
    List.map (Axis2d.withDirection direction) points

-}
withDirection : Direction2d coordinates -> Point2d units coordinates -> Axis2d units coordinates
withDirection givenDirection givenPoint =
    Types.Axis2d { originPoint = givenPoint, direction = givenDirection }


{-| Attempt to construct an axis through the two given points;

    Axis2d.throughPoints p1 p2

is equivalent to

    Maybe.map (Axis2d.through firstPoint)
        (Direction2d.from firstPoint secondPoint)

-}
throughPoints : Point2d units coordinates -> Point2d units coordinates -> Maybe (Axis2d units coordinates)
throughPoints firstPoint secondPoint =
    case Direction2d.from firstPoint secondPoint of
        Just axisDirection ->
            Just (through firstPoint axisDirection)

        Nothing ->
            Nothing


{-| Convert an axis from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Axis2d units1 coordinates -> Axis2d units2 coordinates
at rate (Types.Axis2d axis) =
    Types.Axis2d
        { originPoint = Point2d.at rate axis.originPoint
        , direction = axis.direction
        }


{-| Convert an axis from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Axis2d units1 coordinates -> Axis2d units2 coordinates
at_ rate axis =
    at (Quantity.inverse rate) axis


{-| Get the origin point of an axis.

    Axis2d.originPoint exampleAxis
    --> Point2d.meters 1 3

-}
originPoint : Axis2d units coordinates -> Point2d units coordinates
originPoint (Types.Axis2d axis) =
    axis.originPoint


{-| Get the direction of an axis.

    Axis2d.direction exampleAxis
    --> Direction2d.degrees 30

-}
direction : Axis2d units coordinates -> Direction2d coordinates
direction (Types.Axis2d axis) =
    axis.direction


{-| Find the intersection point between two axes. If none exists (the axes are parallel) then
`Nothing` is returned.
-}
intersectionPoint : Axis2d units coordinates -> Axis2d units coordinates -> Maybe (Point2d units coordinates)
intersectionPoint axis1 axis2 =
    let
        p1 =
            originPoint axis1

        p2 =
            originPoint axis2

        d1 =
            Direction2d.unwrap (direction axis1)

        d2 =
            Direction2d.unwrap (direction axis2)

        crossProduct =
            d1.x * d2.y - d1.y * d2.x
    in
    if crossProduct == 0 then
        Nothing

    else
        let
            distance1 =
                Point2d.signedDistanceFrom axis2 p1

            distance2 =
                Point2d.signedDistanceFrom axis1 p2
        in
        if Quantity.abs distance1 |> Quantity.lessThanOrEqualTo (Quantity.abs distance2) then
            Just (Point2d.along axis1 (distance1 |> Quantity.divideBy crossProduct))

        else
            Just (Point2d.along axis2 (distance2 |> Quantity.divideBy -crossProduct))


{-| Attempt to find the intersection of an axis with a circle. The two points
will be in order of signed distance along the axis. Returns `Nothing` if there
is no intersection.
-}
intersectionWithCircle : Circle2d units coordinates -> Axis2d units coordinates -> Maybe ( Point2d units coordinates, Point2d units coordinates )
intersectionWithCircle (Types.Circle2d { centerPoint, radius }) (Types.Axis2d axis) =
    let
        circleCenterToOrigin =
            Vector2d.from centerPoint axis.originPoint

        (Types.Vector2d cto) =
            circleCenterToOrigin

        ctoLengthSquared =
            cto.x ^ 2 + cto.y ^ 2

        (Quantity dotProduct) =
            Vector2d.componentIn axis.direction circleCenterToOrigin

        (Quantity r) =
            radius

        inRoot =
            dotProduct ^ 2 - ctoLengthSquared + r ^ 2
    in
    if inRoot < 0 then
        Nothing

    else
        let
            d1 =
                -dotProduct - sqrt inRoot

            d2 =
                -dotProduct + sqrt inRoot
        in
        Just
            ( Point2d.along axis (Quantity d1)
            , Point2d.along axis (Quantity d2)
            )


{-| Reverse the direction of an axis while keeping the same origin point.
-}
reverse : Axis2d units coordinates -> Axis2d units coordinates
reverse (Types.Axis2d axis) =
    through axis.originPoint (Direction2d.reverse axis.direction)


{-| Move an axis so that it has the given origin point but unchanged direction.
-}
moveTo : Point2d units coordinates -> Axis2d units coordinates -> Axis2d units coordinates
moveTo newOrigin axis =
    through newOrigin (direction axis)


{-| Rotate an axis around a given center point by a given angle. Rotates the
axis' origin point around the given point by the given angle and the axis'
direction by the given angle.
-}
rotateAround : Point2d units coordinates -> Angle -> Axis2d units coordinates -> Axis2d units coordinates
rotateAround centerPoint angle =
    let
        rotatePoint =
            Point2d.rotateAround centerPoint angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
    \(Types.Axis2d axis) ->
        through (rotatePoint axis.originPoint) (rotateDirection axis.direction)


{-| Rotate an axis around its own origin point by the given angle.
-}
rotateBy : Angle -> Axis2d units coordinates -> Axis2d units coordinates
rotateBy angle axis =
    through (originPoint axis) (Direction2d.rotateBy angle (direction axis))


{-| Translate an axis by a given displacement. Applies the given displacement to
the axis' origin point and leaves the direction unchanged.
-}
translateBy : Vector2d units coordinates -> Axis2d units coordinates -> Axis2d units coordinates
translateBy vector (Types.Axis2d axis) =
    through (Point2d.translateBy vector axis.originPoint) axis.direction


{-| Translate an axis in a given direction by a given distance.
-}
translateIn : Direction2d coordinates -> Quantity Float units -> Axis2d units coordinates -> Axis2d units coordinates
translateIn translationDirection distance axis =
    translateBy (Vector2d.withLength distance translationDirection) axis


{-| Mirror one axis across another. The axis to mirror across is given first and
the axis to mirror is given second.

    Axis2d.mirrorAcross Axis2d.x exampleAxis
    --> Axis2d.through (Point2d.meters 1 -3)
    -->     (Direction2d.degrees -30)

-}
mirrorAcross : Axis2d units coordinates -> Axis2d units coordinates -> Axis2d units coordinates
mirrorAcross otherAxis (Types.Axis2d axis) =
    through (Point2d.mirrorAcross otherAxis axis.originPoint)
        (Direction2d.mirrorAcross otherAxis axis.direction)


{-| Take an axis defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.
-}
relativeTo : Frame2d units globalCoordinates { defines : localCoordinates } -> Axis2d units globalCoordinates -> Axis2d units localCoordinates
relativeTo frame (Types.Axis2d axis) =
    through (Point2d.relativeTo frame axis.originPoint)
        (Direction2d.relativeTo frame axis.direction)


{-| Take an axis defined in local coordinates relative to a given reference
frame, and return that axis expressed in global coordinates.
-}
placeIn : Frame2d units globalCoordinates { defines : localCoordinates } -> Axis2d units localCoordinates -> Axis2d units globalCoordinates
placeIn frame (Types.Axis2d axis) =
    through (Point2d.placeIn frame axis.originPoint)
        (Direction2d.placeIn frame axis.direction)
