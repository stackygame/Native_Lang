# native_lang.py
import os, ast, operator, importlib.util

stack = []
variables = {}
modules = {}

ops = { '+': operator.add, '-': operator.sub, '*': operator.mul, '/': operator.floordiv }

def safe_eval(expr):
    try:
        return int(eval(expr, {"__builtins__": None}, {}))
    except:
        return expr.strip('"')

def resolve_expr(tokens):
    if '"' in tokens or "'" in tokens:
        return tokens.strip('"').strip("'")
    for op_sym in ops:
        if op_sym in tokens:
            left, right = map(str.strip, tokens.split(op_sym))
            return ops[op_sym](int(left), int(right))
    return safe_eval(tokens)

def parse_attr_call(expr):
    if "." in expr:
        parts = expr.split(".")
        mod = modules.get(parts[0])
        if "[" in parts[1]:
            func_name, val = parts[1].split("[")
            val = int(val.rstrip("]"))
            getattr(mod, func_name)(val)
            if len(parts) > 2:
                getattr(mod, parts[2])()
        else:
            if hasattr(mod, parts[1]):
                getattr(mod, parts[1])()

def interpret(lines):
    ip = 0
    while ip < len(lines):
        line = lines[ip].strip()
        if not line or line.startswith("#"):
            ip += 1
            continue

        if line.startswith("IMPORT"):
            modname = line.split()[1]
            mod_path = f"modules/{modname}.nvmod"
            spec = importlib.util.spec_from_file_location(modname, mod_path)
            mod = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(mod)
            modules[modname] = mod

        elif line.startswith("PRINT"):
            expr = line[6:]
            val = resolve_expr(expr)
            print(val)

        elif line.startswith("PUSH"):
            val = resolve_expr(line[5:])
            stack.append(val)

        elif line.startswith("POP"):
            if stack: stack.pop()

        elif line.startswith("DUP"):
            if stack: stack.append(stack[-1])

        elif line.startswith("ADD"):  # assumes top 2 are ints
            b, a = int(stack.pop()), int(stack.pop())
            stack.append(a + b)

        elif line.startswith("SUB"):
            b, a = int(stack.pop()), int(stack.pop())
            stack.append(a - b)

        elif line.startswith("MUL"):
            b, a = int(stack.pop()), int(stack.pop())
            stack.append(a * b)

        elif line.startswith("DIV"):
            b, a = int(stack.pop()), int(stack.pop())
            stack.append(a // b if b else 0)

        elif line.startswith("AND"):
            b, a = stack.pop(), stack.pop()
            stack.append(bool(a and b))

        elif line.startswith("OR"):
            b, a = stack.pop(), stack.pop()
            stack.append(bool(a or b))

        elif line.startswith("NOT"):
            a = stack.pop()
            stack.append(not a)

        elif line.startswith("STCK.EQ.0"):
            if len(stack) > 0 and stack[-1] == 0:
                pass
            else:
                while ip < len(lines) and not lines[ip].strip() == "ENDIF":
                    ip += 1

        elif line.startswith("STCK.GT.0"):
            if len(stack) > 0 and stack[-1] > 0:
                pass
            else:
                while ip < len(lines) and not lines[ip].strip() == "ENDIF":
                    ip += 1

        elif line.startswith("IF"):
            condition = line[3:]
            cond_met = "True" in condition or eval(condition, {}, {})
            if not cond_met:
                while ip < len(lines) and not lines[ip].strip() == "ENDIF":
                    ip += 1

        elif line.startswith("ENDIF"):
            pass

        elif line.startswith("LOOP"):
            count = int(resolve_expr(line.split()[1]))
            loop_block = []
            ip += 1
            while ip < len(lines) and not lines[ip].strip() == "ENDLOOP":
                loop_block.append(lines[ip])
                ip += 1
            for _ in range(count):
                interpret(loop_block)

        elif "." in line:
            parse_attr_call(line.strip())

        ip += 1
