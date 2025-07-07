<h1>NativeLang is a lightweight, stack-oriented scripting language designed for raw expression, modular control, and creative system building. Think of it as part assembly, part pseudocode—fully yours.</h1>
                                                                                                                                                                                                                
Whether you're crafting terminal-based toys, game logic engines, or elegant low-level DSLs, NativeLang invites you to script like you’re building a machine with your hands.

Why NativeLang?
🧠 Stack-aware, not stack-overwhelming Push values, pop logic, and reason like a minimalist CPU.

✍️ Expression-powered Say things like: PRINT "Game Over" or PRINT 1 + 2 * 3

🎛️ Systemic modularity Use IMPORT NativeGameDev to tap into specialized libraries like NativeGameDev.Square[32].player.

🧩 Package Manager: nvcdown Lightweight CLI installer for core and community modules.

⚡ CLI-first: nvc Run, build, and interact from any terminal with zero overhead.

Sample: hello.nvl
PRINT "🌿 Welcome to NativeLang"
PUSH 2
PUSH 3
ADD
PRINT "🌿 Welcome to NativeLang v1.0"
PRINT "— Core Features Showcase —"

# 🎒 Stack Operations
PUSH 10
PUSH 5
ADD
PRINT    # should print 15

PUSH 3
MUL
PRINT    # should print 45

PUSH 9
PUSH 2
SUB
PRINT    # should print 7

PUSH 12
PUSH 4
DIV
PRINT    # should print 3

# 🎛 Logical Operations
PUSH 1
PUSH 0
AND
PRINT    # should print 0 (False)

PUSH 1
PUSH 0
OR
PRINT    # should print 1 (True)

PUSH 0
NOT
PRINT    # should print True (not 0)

# 🔁 Loops
PRINT "Now looping 3 times:"
LOOP 3
    PRINT "🌱 Grow"
ENDLOOP

# 🔍 Conditionals using STCK.EQ.0
PUSH 0
IF STCK.EQ.0
    PRINT "Stack equals 0!"
ENDIF

PUSH 5
IF STCK.GT.0
    PRINT "Top of stack is greater than 0!"
ENDIF

# 🧪 IF with expressions
IF 2 + 2 == 4
    PRINT "2 + 2 = 4 confirmed."
ENDIF

# 🗃 Stack Utilities
DUP
PRINT      # duplicate and print same value

POP
PRINT      # will show the remaining top

# 📦 Module Import + Call Example
IMPORT NativeGameDev
PRINT "🕹 Module NativeGameDev imported"

NativeGameDev.Square[32].player

IF NativeGameDev.ArrowKeys:
    NativeGameDev.Move("player", "left")
ENDIF





<h1>Built by StackyGame</h1>
The brain behind NativeLang is @stackygame—a maker who blends terminal OS builds, interpreter engines, pixel logic, and creative scripting. NativeLang is a culmination of that philosophy: compact, expressive, command-driven code with soul.
