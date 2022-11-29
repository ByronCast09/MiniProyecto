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

def Simpson1(a: Int , b: Int): Double = (b-a) * (((-Math.pow(a, 2)+8*a-12)+(4*(-Math.pow((a+b)/2, 2)+8*(a+b)/2-12))+(-Math.pow(b, 2)+8*b-12))/6)
def Simpson2(a: Int, b: Int): Double = (b-a) * (3*math.pow(a,2)+3*math.pow((a+b)/2,2)+3*math.pow(b,2))/6
def Simpson3(a: Int, b: Int): Double = 0

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
@main def app(): Unit=
  //println(Simpson1(3,5))

  //funcion 1

  println(SCompuesta2(3,5,30,x =>(-Math.pow(x,2)+ (8*x)-12)))
  println(S_extendida(3,5,x =>(-Math.pow(x,2)+ 8 *x-12)))

  println("-------------------------------------------------------\n")
  //funcion 2

  println(SCompuesta2(0,2,20,x =>( 3 * Math.pow(x, 2))))
  println(S_extendida(0,2,x =>( 3 * Math.pow(x, 2))))

  println("-------------------------------------------------------\n")
  //Funcion 3

  println(SCompuesta2(-1,1,10,x=>(x + 2* Math.pow(x,2)- Math.pow(x,3)+5*Math.pow(x,4))))
  println(S_extendida(-1,1,x=>(x + 2* Math.pow(x,2)- Math.pow(x,3)+5*Math.pow(x,4))))

  println("-------------------------------------------------------\n")
  //Funcion 4

  println(SCompuesta2(1,2,4,x=>(2*x+1)/(Math.pow(x,2)+x)))
  println(S_extendida(1,2,x=>(2*x+1)/(Math.pow(x,2)+x)))

  println("-------------------------------------------------------\n")
  //Funcion 5

  println(SCompuesta2(0,1,6,x=>(math.pow(Math.E,x))))
  println(S_extendida(0,1,x=>(math.pow(Math.E,x))))

  println("-------------------------------------------------------\n")
 //Funcion 6

  println(SCompuesta2(2,3,8,x=>(1/ Math.sqrt(x-1))))
  println(S_extendida(2,3,x=>(1/ Math.sqrt(x-1))))

  println("-------------------------------------------------------\n")
  //Funcion 7

  println(SCompuesta2(0,1,2,x=>(1 / (1+Math.pow(x,2)))))
  println(S_extendida(0,1,x=>(1 / (1+Math.pow(x,2)))))

  println("-------------------------------------------------------\n")
