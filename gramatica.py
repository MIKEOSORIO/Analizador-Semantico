#Se importan las librerias
import ply.lex as lex
import ply.yacc as yacc
from colorama import init, Fore

# Declramos los tokens
contador = 0
tokens  = (
    'ID',
    'STRING',
    'INT',
    'DOUBLE',
    'FLOAT',
    'BOOLEAN',
    'PARIZQ',
    'PARDER',
    'CORIZQ',
    'CORDER',
    'IGUAL',
    'MAS',
    'MENOS',
    'POR',
    'DIVIDIDO',
    'MENQUE',
    'MAYQUE',
    'IGUALQUE',
    'NIGUALQUE',
    'TRUE',
    'FALSE',
    'CADENA',
    'DECIMAL',
    'ENTERO',
    'PTCOMA'
)

#Tokens
t_PARIZQ    = r'\('
t_PARDER    = r'\)'
t_CORIZQ    = r'\['
t_CORDER    = r'\]'
t_IGUAL     = r'='
t_MAS       = r'\+'
t_MENOS     = r'-'
t_POR       = r'\*'
t_DIVIDIDO  = r'/'
t_PTCOMA    = r';'
t_MENQUE    = r'<'
t_MAYQUE    = r'>'
t_IGUALQUE  = r'=='
t_NIGUALQUE = r'!='

#Cadenas de texto
def t_CADENA(t):
    r'\".*?\"'
    t.value = t.value[1:-1] # Se remueven las comillas
    return t 

#Función para int
def t_INT(t):
     r'int'
     return t

#Función para string
def t_STRING(t):
     r'String'
     return t

#Función para double
def t_DOUBLE(t):
     r'double'
     return t

#Función para float
def t_FLOAT(t):
     r'float'
     return t

#Función para boolean
def t_BOOLEAN(t):
     r'boolean'
     return t

#Función para false
def t_FALSE(t):
     r'false'
     return t

#Función para true
def t_TRUE(t):
     r'true'
     return t
#Se define esta función exclusivo para números enteros
def t_ENTERO(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print("Valor entero fuera de rango %d", t.value)
        t.value = 0
    return t

#Definimos esta función para nombre de variables
def t_ID(t):
     r'[a-zA-Z_][a-zA-Z_0-9]*'
     return t

#Definimos esta función exclusivamente para decimales
def t_DECIMAL(t):
    r'\d+\.\d+'
    try:
        t.value = float(t.value)
    except ValueError:
        print("Valor flotante fuera de rango %d", t.value)
        t.value = 0
    return t


#Caracteres que vamos a ignorar
t_ignore = " \t"

#Se define esta función para comentarios
def t_COMENTARIO_SENCILLO(t):
    r'//.*\n'
    t.lexer.lineno += 1

#Se define esta función para los salto de línea
def t_nuevalinea(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")


#Se define esta función para carácteres ilegales
def t_error(t):
    print("Carácter Ilegal '%s'" % t.value[0])
    t.lexer.omitir(1)
    resultado = t.lexer.omitir(1)
    print(resultado)
    
#Asociación de operadores y precedencia
precedence = (
    ('left','MAS','MENOS'),
    ('left','POR','DIVIDIDO'),
    ('right','UMENOS'),
    )

#Se declaran varias funciones que nos servirán para poder validar los errores sintacticos.


def p_instrucciones_lista(t):
    '''instrucciones    : instruccion instrucciones
                        | instruccion '''

#Se define esta función para la declaración y asignación de variables
def p_instrucciones_variables(t):
    '''instruccion : INT ID IGUAL expresion2 PTCOMA
                   | INT ID IGUAL ENTERO PTCOMA
                   | STRING ID IGUAL CADENA PTCOMA
                   | STRING ID IGUAL CADENA MAS CADENA PTCOMA
                   | DOUBLE ID IGUAL DECIMAL PTCOMA
                   | DOUBLE ID IGUAL expresion PTCOMA
                   | FLOAT ID IGUAL DECIMAL PTCOMA
                   | FLOAT ID IGUAL expresion PTCOMA
                   | BOOLEAN ID IGUAL expresion3 PTCOMA
                   | INT ID PTCOMA 
                   | STRING ID PTCOMA
                   | DOUBLE ID PTCOMA
                   | FLOAT ID PTCOMA 
                   | BOOLEAN ID PTCOMA '''
                
#Se define esta función para expresiones binarias, ya sea con enteros o decimales
def p_expresion_binaria(t):
    '''expresion : expresion MAS expresion
                  | expresion MENOS expresion
                  | expresion POR expresion
                  | expresion DIVIDIDO expresion'''
    if t[2] == '+'  : t[0] = t[1] + t[3]
    elif t[2] == '-': t[0] = t[1] - t[3]
    elif t[2] == '*': t[0] = t[1] * t[3]
    elif t[2] == '/': t[0] = t[1] / t[3]

#Se define esta función para las expresiones binarias unicamente con números enteros
def p_expresion_binariaInt(t):
    '''expresion2 : ENTERO MAS ENTERO
                  | ENTERO MENOS ENTERO
                  | ENTERO POR ENTERO
                  | ENTERO DIVIDIDO ENTERO'''
    if t[2] == '+'  : t[0] = t[1] + t[3]
    elif t[2] == '-': t[0] = t[1] - t[3]
    elif t[2] == '*': t[0] = t[1] * t[3]
    elif t[2] == '/': t[0] = t[1] / t[3]

#Se define esta función para las expresiones booleanas con signos
def p_expresion_booleana(t):
    '''expresion3 : expresion MAYQUE expresion
                  | expresion MENQUE expresion
                  | expresion IGUALQUE expresion
                  | expresion NIGUALQUE expresion'''
    if t[2] == '>'  : t[0] = t[1] > t[3]
    elif t[2] == '<': t[0] = t[1] < t[3]
    elif t[2] == '==': t[0] = t[1] == t[3]
    elif t[2] == '!=': t[0] = t[1] != t[3]

#Se define esta función para las expresiones true y false
def p_expresion_booleana2(t):
    '''expresion3 : TRUE
                  | FALSE'''

#Se define esta función para los operadores unarias
def p_operadores_unarias(t):
    'expresion : MENOS expresion %prec UMENOS'
    t[0] = -t[2]

#Se define esta función para las expresiones entre paréntesis
def p_expresion_parentesis(t):
    'expresion : PARIZQ expresion PARDER'
    t[0] = t[2]

#Se define esta función para las expresiones de números enteros entre paréntesis
def p_expresion_parentesis2(t):
    'expresion2 : PARIZQ expresion2 PARDER'
    t[0] = t[2]

#Se define esta función para los valores numéricos de tipo entero y decimal
def p_expresion_numero(t):
    '''expresion    : ENTERO
                    | DECIMAL'''
    t[0] = t[1]

#Se define esta función para los posibles errores detectados durante el análisis
def p_error(t):
        print("Error de asignación en '%s'" % t.value)
        global contador 
        contador = 1
    
#Se construye el analizador léxico y sintáctico
lexer = lex.lex()
parser = yacc.yacc() 

f = open("./prueba.txt", "r") #Se abre el archivo .txt
input = f.read()



result = parser.parse(input, tracking=False)

if (contador==1):print(Fore.RED + "COMPILACION NO EXITOSA")

else: print(Fore.BLUE + "COMPILACIÓN EXITOSA")