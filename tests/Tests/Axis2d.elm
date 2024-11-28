module Tests.Axis2d exposing
    ( directionExample
    , intersectionPoint
    , intersectionWithCircle
    , mirrorAcrossExample
    , moveToExample
    , originPointExample
    , placeInExample
    , relativeToExample
    , reverseExample
    , rotateAroundExample
    , throughPoints
    , translateByExample
    , xExample
    , yExample
    )

import Angle
import Axis2d
import Circle2d
import Direction2d
import Expect
import Frame2d
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (meters)
import Point2d
import Quantity
import Test exposing (Test)
import Test.Random as Test
import Vector2d


xExample : Test
xExample =
    Test.test "Axis2d.x example" <|
        \() ->
            Axis2d.x
                |> Expect.axis2d (Axis2d.through Point2d.origin Direction2d.x)


yExample : Test
yExample =
    Test.test "Axis2d.y example" <|
        \() ->
            Axis2d.y
                |> Expect.axis2d (Axis2d.through Point2d.origin Direction2d.y)


originPointExample : Test
originPointExample =
    Test.test "Axis2d.originPoint example" <|
        \() ->
            Axis2d.originPoint Axis2d.x |> Expect.point2d Point2d.origin


directionExample : Test
directionExample =
    Test.test "Axis2d.direction example" <|
        \() ->
            Axis2d.direction Axis2d.y |> Expect.direction2d Direction2d.y


reverseExample : Test
reverseExample =
    Test.test "Axis2d.reverse example" <|
        \() ->
            Axis2d.reverse Axis2d.x
                |> Expect.axis2d
                    (Axis2d.through Point2d.origin Direction2d.negativeX)


moveToExample : Test
moveToExample =
    Test.test "Axis2d.moveTo example" <|
        \() ->
            let
                axis =
                    Axis2d.withDirection Direction2d.y
                        (Point2d.fromTuple meters ( 2, 3 ))

                newOrigin =
                    Point2d.fromTuple meters ( 4, 5 )
            in
            Axis2d.moveTo newOrigin axis
                |> Expect.axis2d
                    (Axis2d.withDirection Direction2d.y
                        (Point2d.fromTuple meters ( 4, 5 ))
                    )


rotateAroundExample : Test
rotateAroundExample =
    Test.test "Axis2d.rotateAround example" <|
        \() ->
            Axis2d.rotateAround Point2d.origin (Angle.degrees 90) Axis2d.x
                |> Expect.axis2d Axis2d.y


translateByExample : Test
translateByExample =
    Test.test "Axis2d.translateBy example" <|
        \() ->
            let
                displacement =
                    Vector2d.fromTuple meters ( 2, 3 )
            in
            Axis2d.translateBy displacement Axis2d.y
                |> Expect.axis2d
                    (Axis2d.withDirection Direction2d.y
                        (Point2d.fromTuple meters ( 2, 3 ))
                    )


mirrorAcrossExample : Test
mirrorAcrossExample =
    Test.test "Axis2d.mirrorAcross example" <|
        \() ->
            let
                axis =
                    Axis2d.through
                        (Point2d.fromTuple meters ( 1, 2 ))
                        (Direction2d.fromAngle (Angle.degrees 30))
            in
            Axis2d.mirrorAcross Axis2d.x axis
                |> Expect.axis2d
                    (Axis2d.through
                        (Point2d.fromTuple meters ( 1, -2 ))
                        (Direction2d.fromAngle (Angle.degrees -30))
                    )


relativeToExample : Test
relativeToExample =
    Test.test "Axis2d.relativeTo example" <|
        \() ->
            let
                originPoint =
                    Point2d.fromTuple meters ( 2, 3 )
            in
            Axis2d.relativeTo (Frame2d.atPoint originPoint) Axis2d.x
                |> Expect.axis2d
                    (Axis2d.withDirection Direction2d.x
                        (Point2d.fromTuple meters ( -2, -3 ))
                    )


placeInExample : Test
placeInExample =
    Test.test "Axis2d.placeIn example" <|
        \() ->
            let
                originPoint =
                    Point2d.fromTuple meters ( 2, 3 )
            in
            Axis2d.placeIn (Frame2d.atPoint originPoint) Axis2d.x
                |> Expect.axis2d
                    (Axis2d.withDirection Direction2d.x
                        (Point2d.fromTuple meters ( 2, 3 ))
                    )


throughPoints : Test
throughPoints =
    Test.check2 "throughPoints"
        Random.point2d
        Random.point2d
        (\firstPoint secondPoint ->
            case Axis2d.throughPoints firstPoint secondPoint of
                Just axis ->
                    Expect.all
                        [ \() ->
                            Axis2d.originPoint axis |> Expect.point2d firstPoint
                        , \() ->
                            Point2d.signedDistanceFrom axis secondPoint
                                |> Expect.quantity Quantity.zero
                        ]
                        ()

                Nothing ->
                    firstPoint |> Expect.point2d secondPoint
        )


intersectionPoint : Test
intersectionPoint =
    Test.check2 "intersectionPoint"
        Random.axis2d
        Random.axis2d
        (\firstAxis secondAxis ->
            let
                -- Check how close the axes are to being parallel
                differenceFromParallel =
                    Direction2d.angleFrom (Axis2d.direction firstAxis) (Axis2d.direction secondAxis)
                        |> Quantity.fractionalModBy (Angle.degrees 180)
                        |> Quantity.abs
            in
            if differenceFromParallel |> Quantity.greaterThan (Angle.degrees 1) then
                -- Axes are not close to parallel, check for a valid intersection
                case Axis2d.intersectionPoint firstAxis secondAxis of
                    Just point ->
                        -- Check that the returned point is in fact on both axes
                        let
                            maxError =
                                Quantity.max
                                    (Quantity.abs (Point2d.signedDistanceFrom firstAxis point))
                                    (Quantity.abs (Point2d.signedDistanceFrom secondAxis point))
                        in
                        maxError |> Expect.quantity Quantity.zero

                    Nothing ->
                        Expect.fail "Expected axis intersection point but got Nothing"

            else
                -- Axes are almost parallel, don't check intersection point (since it will be
                -- numerically badly behaved)
                Expect.pass
        )


intersectionWithCircle : Test
intersectionWithCircle =
    Test.describe "intersectionWithCircle"
        [ Test.test "no intersection points" <|
            \_ ->
                let
                    sphere =
                        Circle2d.withRadius (meters 1) (Point2d.meters 0 0)

                    axis =
                        Axis2d.through (Point2d.meters 6 0) Direction2d.y
                in
                Expect.equal (Axis2d.intersectionWithCircle sphere axis) Nothing
        , Test.test "two intersection points" <|
            \_ ->
                let
                    sphere =
                        Circle2d.withRadius (meters 1) (Point2d.meters 0 0)

                    axis =
                        Axis2d.through (Point2d.meters 0 0) Direction2d.y
                in
                Expect.equal (Axis2d.intersectionWithCircle sphere axis)
                    (Just ( Point2d.meters 0 -1, Point2d.meters 0 1 ))
        , Test.test "the same intersection points" <|
            \_ ->
                let
                    sphere =
                        Circle2d.withRadius (meters 1) (Point2d.meters 0 0)

                    axis =
                        Axis2d.through (Point2d.meters 1 0) Direction2d.y
                in
                Expect.equal (Axis2d.intersectionWithCircle sphere axis)
                    (Just ( Point2d.meters 1 0, Point2d.meters 1 0 ))
        , Test.check2 "intersection points should be on the circle and the axis"
            Random.axis2d
            Random.circle2d
            (\axis circle ->
                case Axis2d.intersectionWithCircle circle axis of
                    Just pointPair ->
                        let
                            -- An intersection point should be on the circle
                            -- (have a distance from the circle center point
                            -- equal to the circle radius), and on the axis
                            -- (have a zero distance from the axis)
                            validIntersectionPoint point =
                                Expect.all
                                    [ Point2d.distanceFrom (Circle2d.centerPoint circle)
                                        >> Expect.quantity (Circle2d.radius circle)
                                    , Point2d.signedDistanceFrom axis
                                        >> Expect.quantity Quantity.zero
                                    ]
                                    point
                        in
                        -- Both intersection points should be valid
                        Expect.all
                            [ Tuple.first >> validIntersectionPoint
                            , Tuple.second >> validIntersectionPoint
                            ]
                            pointPair

                    Nothing ->
                        -- If the axis does not intersect the circle, then the
                        -- absolute distance from the circle center point to the axis
                        -- should be greater than the radius of the circle (if
                        -- it was less, then there should be an intersection!)
                        Circle2d.centerPoint circle
                            |> Point2d.signedDistanceFrom axis
                            |> Quantity.abs
                            |> Expect.quantityGreaterThan (Circle2d.radius circle)
            )
        ]
