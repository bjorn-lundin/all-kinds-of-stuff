/**
   BasicHTTPSClient.ino
    Created on: 20.08.2018
*/
#include <Arduino.h>
#include <ESP8266WiFi.h>
#include <ESP8266WiFiMulti.h>
#include <ESP8266HTTPClient.h>
#include <WiFiClientSecureBearSSL.h>
#include <Wire.h>
#include <SPI.h>
#include <Adafruit_Sensor.h>
#include "Adafruit_BME680.h"

// global variables

#define SEALEVELPRESSURE_HPA (1013.25)
Adafruit_BME680 bme; // I2C

ESP8266WiFiMulti WiFiMulti;
int counter = 0;
#define SensorPin A0 
unsigned long now;
const String chipid = String(ESP.getChipId(), HEX);
// local functions start

int do_measurement() {

  // Tell BME680 to begin measurement.
  unsigned long endTime = bme.beginReading();
  if (endTime == 0) {
    Serial.println(F("Failed to begin reading :("));
    return 2;
  }
  Serial.print(F("Reading started at "));
  Serial.print(millis());
  Serial.print(F(" and will finish at "));
  Serial.println(endTime);

  Serial.println(F("You can do other work during BME680 measurement."));
  delay(50); // This represents parallel work.
  // There's no need to delay() until millis() >= endTime: bme.endReading()
  // takes care of that. It's okay for parallel work to take longer than
  // BME680's measurement time.

  // Obtain measurement results from BME680. Note that this operation isn't
  // instantaneous even if milli() >= endTime due to I2C/SPI latency.
  if (!bme.endReading()) {
    Serial.println(F("Failed to complete reading :("));
    return 1;
  }
  Serial.print(F("Reading completed at "));
  Serial.println(millis());

  Serial.print(F("Temperature = "));
  Serial.print(bme.temperature);
  Serial.println(F(" *C"));

  Serial.print(F("Pressure = "));
  Serial.print(bme.pressure / 100.0);
  Serial.println(F(" hPa"));

  Serial.print(F("Humidity = "));
  Serial.print(bme.humidity);
  Serial.println(F(" %"));

  Serial.print(F("Gas = "));
  Serial.print(bme.gas_resistance / 1000.0);
  Serial.println(F(" KOhms"));

  Serial.print(F("Approx. Altitude = "));
  Serial.print(bme.readAltitude(SEALEVELPRESSURE_HPA));
  Serial.println(F(" m"));

  Serial.println();

  return 0;
  
}

void do_connect() {
  // wait for WiFi connection
  Serial.printf("[CONNECT] start\n");
  while(WiFiMulti.run() != WL_CONNECTED) {
    Serial.print('.');
    delay(1000);
  }
  Serial.println("conntected to network..");
  Serial.println(WiFi.SSID());              // Tell us what network we're connected to
  Serial.print("IP address:\t");
  Serial.println(WiFi.localIP());           // Send the IP address of the ESP8266 to the computer   
}

// local functions stop
void setup() {

  Serial.begin(115200);
  
  for(uint8_t t = 4; t > 0; t--) {
    Serial.printf("[SETUP] WAIT %d...\n", t);
    Serial.flush();
    delay(1000);
  }
//WiFi stuff
  
  WiFi.mode(WIFI_STA);
  
  Serial.println("adding network 'Telia-36275B'");
  WiFiMulti.addAP("Telia-36275B", "01CC24CABB");
  
  Serial.println("adding network 'mcs'");
  WiFiMulti.addAP("mcs", "mcs bor har");
  do_connect();
  counter = 60;

  //sensor stuff
  Serial.println(F("Set up oversampling and filter initialization"));
  bme.setTemperatureOversampling(BME680_OS_8X);
  bme.setHumidityOversampling(BME680_OS_2X);
  bme.setPressureOversampling(BME680_OS_4X);
  bme.setIIRFilterSize(BME680_FILTER_SIZE_3);
  bme.setGasHeater(320, 150); // 320*C for 150 ms  

  Serial.println(F("BME680 async test"));
  if (!bme.begin(0x76)) {
    Serial.println(F("Could not find a valid BME680 sensor, check wiring!"));
    while (1);
  }
    
}
void loop() {
  counter++ ;
  if (counter < 60) {
    delay(1000);
    return;
  }
  Serial.println("reset counter and do stuff");
  counter = 0 ;
  int level = do_measurement();
  String(ESP.getChipId(), HEX);
  
  // wait for WiFi connection
  if ((WiFiMulti.run() == WL_CONNECTED)) {
  
    std::unique_ptr<BearSSL::WiFiClientSecure>client(new BearSSL::WiFiClientSecure);
    client->setInsecure();
    now = millis();
    
    HTTPClient https;
    String URL = String ("https://lundin.duckdns.org/airquality.html?context=air&chipid=");
    URL += chipid;
    URL += "&temp=";
    URL += bme.temperature;
    URL += "&pressure=";
    URL += bme.pressure;
    URL += "&humidity=";
    URL += bme.humidity;
    URL += "&gasresistance=";
    URL += bme.gas_resistance;
    
    URL += "&ts=";
    URL += now;
    
    Serial.println(URL);
    Serial.println("[HTTPS] begin...");
    if (https.begin(*client, URL)) {  // HTTPS
      Serial.println("[HTTPS] GET...");
      // start connection and send HTTP header
      int httpCode = https.GET();
      // httpCode will be negative on error
      if (httpCode > 0) {
        // HTTP header has been send and Server response header has been handled
        Serial.printf("[HTTPS] GET... code: %d\n", httpCode);
        // file found at server
        if (httpCode == HTTP_CODE_OK || httpCode == HTTP_CODE_CREATED) {
          String payload = https.getString();
          Serial.println(payload);
        } 
      } else {
        Serial.printf("[HTTPS] GET... failed, error: %s\n", https.errorToString(httpCode).c_str());
      }
      https.end();
            
    } else {
      Serial.printf("[HTTPS] Unable to connect\n");
    }
  } else {
    do_connect();
  }
}
