# haskell-wordle

A toy CLI Wordle clone written in Haskell. This started off as a simple way to play around with Haskell modeling. I wanted to get some experience with web development and Haskell web servers - [Spock](https://hackage.haskell.org/package/Spock) specifically - so decided to add a simple front end written in JS. There were also plans to implement a simplified version of the [Wordle Bot](https://www.nytimes.com/interactive/2022/upshot/wordle-bot.html) that could make intelligent guesses based on feedback ('were' being the operative tense as it's unlikely I'll get to this).

### Running the application
Stack is the preferred way of lauching the application. There are several executables included in `package.yaml` - `web`, `wordle-cli`, and `wordle-bot`, so you'll need to specify which one you want to run (e.g. `stack run wordle-cli`).

`stack run web` will launch a web server listening to `localhost:8080`

### Future plans
[TODO.md](./TODO.md) captures where I want to take this application; other ideas I have are to beautify the CLI using [brick](https://hackage.haskell.org/package/brick) or ditch JS entirely and give [hyperbole](https://hackage.haskell.org/package/hyperbole-0.3.6/docs/Web-Hyperbole.html) a look. There are no guarantees any of this work will ever be completed :) Have fun poking around!
