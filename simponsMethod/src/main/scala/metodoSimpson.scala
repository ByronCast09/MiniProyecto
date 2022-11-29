/*
FUNCIONES MATEMATICAS
-x^2+8x-12  a = 3 , b = 5
3x^2 a = 0 , b = 5
x+2x^2-x^3+5x^4   a = -1 , b = 1
(2x+1)/(x^2+x)    a = 1 , b = 2
e^x    a = 0 , b = 1
1/Math.sqr(x-1, 2)    a = 2 , b = 3
1/(1+x^2)        a = 0 ,  b = 1

Metodo de Simpson 1/3
def Simpson(a: Int, b: Int): Double = (b-a) * (f(a)-f(x)-f(b))/6
*/

def Simpson(a:Int, b:Int, fun:Double => Double)=
  val x = (b + a) / 2
  val h = (fun(a)+4*fun(x)+fun(b))/6
  (b-a)*h

/*
Metodo de Simpson 1/3 Compuesta
def Simpson(a: Int, b: Int, n: Int): Double = ((b-a)/n) /
*/
def SCompuesta2(a:Int,b:Int,n:Int,fun:Double=>Double)=
  val h = (b - a) / n.toDouble
  val xj = (1 to (n / 2)).map(j => fun(a + ((2 * j)-2) * h)
    + (4 * fun(a + ((2 * j)-1) * h))
    + fun(a + (2 * j) * h)).sum
  (h / 3) * xj

def S_extendida(a:Int,b:Int,fun:Double=>Double)=
  val n = 2*(b-a)
  val h =(b-a)/n.toDouble
  val j_par = fun(a)+ 4 * (1 to(n-1) by 2).map(j => fun(a + j*h)).sum
  val i_impar = fun(b) + 2 *(2 to (n-2) by 2).map(i => fun(a + i*h)).sum
  (h / 3)*(i_impar + j_par)
/*
Calculo de error
*/

def calcError(valorOb: Double, valorAbs: Double) = {
  (valorOb-valorAbs).abs
}

@main def app(): Unit =
  //println(Simpson1(3,5))

  //funcion 1

  println(Simpson(3,5,x =>(-Math.pow(x,2)+ 8 *x-12)))
  println(SCompuesta2(3,5,30,x =>(-Math.pow(x,2)+ (8*x)-12)))
  println(S_extendida(3,5,x =>(-Math.pow(x,2)+ 8 *x-12)))

  println("-------------------------------------------------------\n")
  //funcion 2

  println(Simpson(0,2,x =>( 3 * Math.pow(x, 2))))
  println(SCompuesta2(0,2,20,x =>( 3 * Math.pow(x, 2))))
  println(S_extendida(0,2,x =>( 3 * Math.pow(x, 2))))

  println("-------------------------------------------------------\n")
  //Funcion 3

  println(Simpson(-1,1,x=>(x + 2* Math.pow(x,2)- Math.pow(x,3)+5*Math.pow(x,4))))
  println(SCompuesta2(-1,1,10,x=>(x + 2* Math.pow(x,2)- Math.pow(x,3)+5*Math.pow(x,4))))
  println(S_extendida(-1,1,x=>(x + 2* Math.pow(x,2)- Math.pow(x,3)+5*Math.pow(x,4))))

  println("-------------------------------------------------------\n")
  //Funcion 4

  println(Simpson(1,2,x=>(2*x+1)/(Math.pow(x,2)+x)))
  println(SCompuesta2(1,2,4,x=>(2*x+1)/(Math.pow(x,2)+x)))
  println(S_extendida(1,2,x=>(2*x+1)/(Math.pow(x,2)+x)))

  println("-------------------------------------------------------\n")
  //Funcion 5

  println(Simpson(0,1,x=>(math.pow(Math.E,x))))
  println(SCompuesta2(0,1,6,x=>(math.pow(Math.E,x))))
  println(S_extendida(0,1,x=>(math.pow(Math.E,x))))

  println("-------------------------------------------------------\n")
  //Funcion 6

  println(Simpson(2,3,x=>(1/ Math.sqrt(x-1))))
  println(SCompuesta2(2,3,8,x=>(1/ Math.sqrt(x-1))))
  println(S_extendida(2,3,x=>(1/ Math.sqrt(x-1))))

  println("-------------------------------------------------------\n")
  //Funcion 7

  println(Simpson(0,1,x=>(1 / (1+Math.pow(x,2)))))
  println(SCompuesta2(0,1,2,x=>(1 / (1+Math.pow(x,2)))))
  println(S_extendida(0,1,x=>(1 / (1+Math.pow(x,2)))))

  println("-------------------------------------------------------\n")

  //Calcula del error
  //Simpsons
  println("Funcion 1: " + calcError(Simpson(3,5, x =>(-Math.pow(x,2)+ 8 *x-12)),7.33))
  println("Funcion 2: " + calcError(Simpson(0,2,x =>( 3 * Math.pow(x, 2))),8.0))
  println("Funcion 3: " + calcError(Simpson(-1,1,x=>(x + 2* Math.pow(x,2)- Math.pow(x,3)+5*Math.pow(x,4))), 3.333))
  println("Funcion 4: " + calcError(Simpson(1,2,x=>(2*x+1)/(Math.pow(x,2)+x)),1.09861))
  println("Funcion 5: " + calcError(Simpson(0,1,x=>(math.pow(Math.E,x))),1.71828))
  println("Funcion 6: " + calcError(Simpson(2,3,x=>(1/ Math.sqrt(x-1))), 0.828427))
  println("Funcion 7: " + calcError(Simpson(0,1,x=>(1 / (1+Math.pow(x,2)))),0.785398))

  println("-------------------------------------------------------\n")

  //Simpsons Compuesta
  println("Funcion 1: " + calcError(SCompuesta2(3,5,30,x =>(-Math.pow(x,2)+ (8*x)-12)), 7.33))
  println("Funcion 2: " + calcError(SCompuesta2(0,2,20,x =>( 3 * Math.pow(x, 2))), 8.0))
  println("Funcion 3: " + calcError(SCompuesta2(-1,1,10,x=>(x + 2* Math.pow(x,2)- Math.pow(x,3)+5*Math.pow(x,4))), 3.333))
  println("Funcion 4: " + calcError(SCompuesta2(1,2,4,x=>(2*x+1)/(Math.pow(x,2)+x)), 1.09861))
  println("Funcion 5: " + calcError(SCompuesta2(0,1,6,x=>(math.pow(Math.E,x))), 1.71828))
  println("Funcion 6: " + calcError(SCompuesta2(2,3,8,x=>(1/ Math.sqrt(x-1))), 0.828427))
  println("Funcion 7: " + calcError(SCompuesta2(0,1,2,x=>(1 / (1+Math.pow(x,2)))), 0.785398))

  println("-------------------------------------------------------\n")

  //Simpsons Extendida
  println("Funcion 1: " + calcError(S_extendida(3,5,x =>(-Math.pow(x,2)+ 8 *x-12)), 7.33))
  println("Funcion 2: " + calcError(S_extendida(0,2,x =>( 3 * Math.pow(x, 2))), 8.0))
  println("Funcion 3: " + calcError(S_extendida(-1,1,x=>(x + 2* Math.pow(x,2)- Math.pow(x,3)+5*Math.pow(x,4))), 3.333))
  println("Funcion 4: " + calcError(S_extendida(1,2,x=>(2*x+1)/(Math.pow(x,2)+x)), 1.09861))
  println("Funcion 5: " + calcError(S_extendida(0,1,x=>(math.pow(Math.E,x))), 1.71828))
  println("Funcion 6: " + calcError(S_extendida(2,3,x=>(1/ Math.sqrt(x-1))), 0.828427))
  println("Funcion 7: " + calcError(SCompuesta2(0,1,2,x=>(1 / (1+Math.pow(x,2)))), 0.785398))



