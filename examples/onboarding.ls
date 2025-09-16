gather io

note solicit_profile():
    let first be io::ask("What is your first name?")
    let last be io::ask("What is your last name?")
    let age be io::ask_as("How many years have you wandered?", "number")
    let savings be io::ask_as("How much rests in your account?", "number")
    halt { first be first, last be last, age be age, savings be savings }

note describe(profile):
    io::echo("Welcome, " + profile["first"] + " " + profile["last"] + ".")
    io::echo("You have journeyed " + profile["age"] + " years.")

    let guidance be "Keep tending your garden."
    if profile["savings"] >= 100000:
        set guidance to "Your coffers are radiant."
    otherwise if profile["savings"] <= 0:
        set guidance to "Time to gather new opportunities."
    io::echo("Savings -> " + profile["savings"])
    io::echo(guidance)
    halt nothing

note main():
    io::echo("Liesel onboarding begins. Share a few details...")
    let profile be solicit_profile()
    io::echo("\nSummary:")
    describe(profile)
    halt nothing

main()
