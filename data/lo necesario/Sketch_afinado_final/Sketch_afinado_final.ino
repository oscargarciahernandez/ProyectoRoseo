//variables y constantes generales
#define PI 3.1415926535897932384626433832795
#define r 0.905/(2*PI) // Radio del aerogenerador
#define x  0.0295 // Longitud de la pegatina reflectante
#define pin_sensor A5 //Pin analógico para el anemómetro
#define pin 2 // Pin digital que medira el el pulso para el encoder
#include <movingAvg.h>// Paquete para hacer "moving average". "Signal smoothing"
int xx=0; 
int yy=0;
int segundos_muestra=60;
int datos_segundo=2; 

//endcoder
movingAvg avgRPM1(2);                  // define the moving average object
float T; //Periodo de paso
float RPM; //RPM del aerogenerador 

float RPM1;
int pulsos=0;
float last_milis=0;
float intervalo=0; 
void encoder() {
  pulsos = pulsos+1; 
  }

  // anemómetro
  movingAvg avgviento(2);                  // define the moving average object
  float adc; //Variable para obtener los valores en el 1 paso
  float voltaje; //Variable para obtener el voltaje en el 2 paso
  float Vviento; //Variable final del sensor en el 3 paso
  float viento_avg;
  float rel_voltaje_variable = 3.039786; //Relación Voltaje/Variable del sensor (en el caso del LM35 es 100)
  

void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600);//Inicializamos comunicación serie
  pinMode(pin, INPUT); // Designamos pin 2 como entrada 
  attachInterrupt(0, encoder, RISING); // 0 = pin2, encoder = funcion a ejecutar, RISING = se ejecuta cuando el pulso aumenta
  avgRPM1.begin();    
  avgviento.begin(); 
  Serial.println("pulsa 1 pa empezar");                       
  Serial.print("RPM");
  Serial.print("  ");
  Serial.println("m/s");
}

void loop() {
  // put your main code here, to run repeatedly:
if (Serial.available() > 0) {
xx = Serial.read();

  while(xx==49){
        
  if(millis()-last_milis >= (1000/datos_segundo)){

  //Encoder, midiendo tiempo de paso
  intervalo=millis()-last_milis;
  T = pulseIn(pin,HIGH,5000000); // Cálculo del periodo del pulso 
  RPM = (x*60*1000000)/(r*2*PI*T); // Cálculos RPM
  RPM1=avgRPM1.reading(RPM);


  // anemómetro
  adc = analogRead(pin_sensor);//lectura analógica anemómetro
  voltaje = adc * 5 / 1023; //conversión lectura voltaje
  Vviento = voltaje * rel_voltaje_variable;// voltaje por relacion **pendiente de calibración
  viento_avg = avgviento.reading(Vviento); 
  
  
  //Salida puerto serie
  Serial.print(RPM1);
  Serial.print("  ");
  Serial.println(Vviento);
  
  
  // Actualizar pulsos, tiempo y numero de dato
  pulsos=0;
  last_milis= millis();
  yy=yy+1;


  //Cuando se alcanza el numero de datos requerido se para la cuenta a la espera de que se introduzca "1" por el puerto serie
 
  }
   if(yy >= segundos_muestra*datos_segundo){
    yy=0;
    xx=0;
    Serial.println("pulsa 1 pa' empezar");                       
    Serial.print("RPM");
    Serial.print("  ");
    Serial.println("m/s");
    }
  }
  
  
  


  }
 
}





