class Main {
  val randInt = (n: Int) => nextRandom % n

  val input = () => {
    print("guess a number: ")
    readInt()
  }

  val main = () => {
    initRandom()
    val answer = randInt(100)
    var chance = 10
    var wrong = true
    while chance > 0 && wrong do {
      val guess = input()
      wrong = guess != answer
      if wrong then {
        chance = chance - 1
        if guess > answer then
          println("Too big.")
        else
          println("Too small.")
        print("You have ")
        printInt(chance)
        println(" chance(s) left.")
      } else {
        println("Correct!")
      }
    }
    if wrong then
      println("You lose!")
    else
      println("You win!")

    print("The answer is ")
    printlnInt(answer)
  }
}