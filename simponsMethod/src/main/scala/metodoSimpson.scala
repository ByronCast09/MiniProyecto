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
def SCompuesta(a: Int, b: Int, n: Int): Double = ((b-a)/n)/3 * ((-Math.pow(a, 2)+8*a-12)+(-Math.pow(b, 2)+8*b-12)
  + (1 to n -1).toList.filter(_ % 2 == 0).map(i => 2*(-Math.pow(a+i*(b-a)/n, 2)+8*a+i*(b-a)/n-12)).sum
  + (1 to n -1).toList.filter(_ % 2 != 0).map(i => 4*(-Math.pow(a+i*(b-a)/n, 2)+8*a+i*(b-a)/n-12)).sum)

def S_extendida(x:Int,y:Int,fun:Double=>Double)={
  val n = 2*(y-x)
  val h =(y-x)/n.toDouble
  val j_par = fun(x)+ 4 * (1 to(n-1) by 2).map(j => fun(x + j*h)).sum
  val i_impar = fun(y) + 2 *(2 to (n-2) by 2).map(i => fun(x + i*h)).sum
  (h / 3)*(i_impar + j_par)
}
@main def app(): Unit=
  println(Simpson1(3,5))
  println(SCompuesta(3,5,10))
  println(Simpson2(0,2))
  val a = 3
  val b = 5
  val n = 4
  val nums = (1 to n-1).toList.filter(_ % 2 != 0).map(i => 4*(-Math.pow(a+i*(b-a)/n, 2)+8*a+i*(b-a)/n-12))
  println(nums)
  val nums1 = (1 to n-1).toList.filter(_ % 2 != 0).map(i => 4*(-Math.pow(a+i*(b-a)/n, 2)))
  println(nums1)
  //----
  println(S_extendida(3,5,x =>(-math.pow(x,2)+ 8 *x-12)))