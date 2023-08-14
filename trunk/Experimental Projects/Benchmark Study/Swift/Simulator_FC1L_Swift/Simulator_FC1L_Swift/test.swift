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

// TP:

let testBrick = TP()
testBrick.G = 5
testBrick.input = 2
print("Test #1 for TP element:   ", terminator: "")
let res = testBrick.simAndGetOutput()
CheckEqual(expected: 5 * 2, received: res)
print("Test #2 for TP element:   ", terminator: "")
let res2 = testBrick.simAndGetOutput()
CheckEqual(expected: 5 * 2, received: res2)
testBrick.amplitude = 2
print("Test #3 for TP element:   ", terminator: "")
let res3 = testBrick.getFR()
CheckEqual(expected: 5 * 2, received: res3.M)

// TPAdd:

let testBrickAdd = TPAdd()
testBrickAdd.G = 1
testBrickAdd.input1 = 2
testBrickAdd.input2 = 11
print("Test for TPAdd element:   ", terminator: "")
let resAdd = testBrickAdd.simAndGetOutput()
CheckEqual(expected: 13, received: resAdd)

// TPSub:

let testBrickSub = TPSub()
testBrickSub.G = 1
testBrickSub.input1 = 107
testBrickSub.input2 = 11
print("Test for TPSub element:   ", terminator: "")
let resSub = testBrickSub.simAndGetOutput()
CheckEqual(expected: 96, received: resSub)

// TPT1:

let testBrickT1_1 = TPT1()
testBrickT1_1.G = 5
testBrickT1_1.delta = 10
testBrickT1_1.t1 = 15
testBrickT1_1.input = 2
testBrickT1_1.x1 = 0
print("Test #1 for TPT1 element: ", terminator: "")
var resT1 = testBrickT1_1.output()
CheckEqual(expected: 0.0, received: resT1)
print("Test #2 for TPT1 element: ", terminator: "")
for _ in 1...100000
{
    resT1 = testBrickT1_1.simAndGetOutput()
    testBrickT1_1.x1 = resT1
}
CheckTrue(received: testBrickT1_1.G * testBrickT1_1.input - testBrickT1_1.x1 < 1e-13)

let testBrickT1_2 = TPT1()
testBrickT1_2.G = 5
testBrickT1_2.delta = 1
testBrickT1_2.t1 = 15e6
testBrickT1_2.input = 2
testBrickT1_2.x1 = 0
print("Test #3 for TPT1 element: ", terminator: "")
resT1 = testBrickT1_2.output()
CheckEqual(expected: 0, received: resT1)
print("Test #4 for TPT1 element: ", terminator: "")
for _ in 1...100000
{
    resT1 = testBrickT1_2.simAndGetOutput()
    testBrickT1_2.x1 = resT1
}
CheckTrue(received: testBrickT1_2.x1 < 0.07)