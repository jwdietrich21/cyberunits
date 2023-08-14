import Foundation

func CheckEqual(expected: Double, received: Double)
{
    if received == expected
    {
        print("passed")
    } else
    {
        print("failed")
        fatalError("failed")
    }
}

func CheckTrue(received: Bool)
{
    if received
    {
        print("passed")
    } else {
        print("failed")
        fatalError("failed")
    }
}

print("Unit tests:")

// TASIA:

print("Test for TASIA element:   ", terminator: "")

let alpha = 10.0
let beta = 0.5
var resASIA: Double

let testBrickTASIA = TASIA()
testBrickTASIA.setAlpha(alpha)
testBrickTASIA.setBeta(beta)
testBrickTASIA.setDelta(1)
testBrickTASIA.input = 1.0
for _ in 1...100
{
    resASIA = testBrickTASIA.simAndGetOutput()
    testBrickTASIA.setX1(resASIA)
}
CheckTrue(received: alpha / beta - testBrickTASIA.PT1Analog.x1 < 0.07)

// TMiMe:

print("Test for TMiMe element:   ", terminator: "")

let G = 5.0
let D = 2.0
let testSignal = 10.0
var resMiMe: Double

let testBrickMiMe = TMiMe()
testBrickMiMe.G = G
testBrickMiMe.D = D
testBrickMiMe.input = testSignal
resMiMe = testBrickMiMe.simAndGetOutput()
CheckEqual(expected: G * testSignal / (D + testSignal), received: resMiMe)

// TNoCoDI:

print("Test for TNoCoDI element: ", terminator: "")
let xe1 = 5.0
let xe2 = 4.0
var resNoCoDI: Double

let testBrickNoCoDI = TNoCoDI()
testBrickNoCoDI.input1 = xe1
testBrickNoCoDI.input2 = xe2
resNoCoDI = testBrickNoCoDI.simAndGetOutput()
CheckEqual(expected: xe1 / (1 + xe2), received: resNoCoDI)
