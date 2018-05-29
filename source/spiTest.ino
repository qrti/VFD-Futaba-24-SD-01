// spiTest for vfdFu24 qrt@qland.de 180520

#define SPIPARS			2000000, MSBFIRST, SPI_MODE3

#define NUMDIGITS		24                      // number of digits
#define BRISTEPS		64                      // brightness steps
												
#define CMD_SETCUR		0                       // command set cursor 0..NUMDIGITS-1
												
#define CCO_FIRST		32                      // char code first
#define CCO_CUST0		128                     //           custom 0..8 (unused)
#define CCO_LAST		135                     //           last
												
#define CMD_CLRHOME		136                     // command clear home
#define CMD_SUPON		137                     //         supply on
#define CMD_SUPOFF		138                     //                off
#define CMD_SETCUCH     139                     //         set custom char
#define CMD_SCLON       140                     //         scroll on
#define CMD_SCLOFF      141                     //                off

												
#define CMD_BRIGHT		192                     // command brightness 192..255 (64 steps)

#define DEGCEL			128						// custom char degree celsius
uint8_t CUCH0[] = { 0x60, 0x60, 0x3c, 0x42, 0x24 };

#include <SPI.h>								// 50 MISO, 51 MOSI, 52 SCK, 53 SS

SPISettings settingsA(SPIPARS);

void setup()
{
	Serial.begin(9600);
	SPI.begin();

	delay(1000);

	SPI.beginTransaction(settingsA);
	//SPI.endTransaction();

	setCustomChar(0, CUCH0);

	sendByte(CMD_SUPON);
	sendByte(CMD_SCLON);				// includes CLR_HOME
	sendByte(CMD_BRIGHT+32);
}

void loop()
{
	static char buf[32];
	static uint32_t i;

	if(i % 100 == 0){
		sprintf(buf, "%010lu\n", i);
		Serial.write(buf);
	}

	sprintf(buf, "%010lu %c", i++, DEGCEL);
	printScroll(buf);

	delay(1000);
}

void printScroll(const char *s)
{
	static char o[32];

	for(int i=0; i<strlen(s); i++){
		if(s[i] != o[i]){
			printCharAt(s[i], i);
			o[i] = s[i];
		}
	}
}

void printCharAt(char c, uint8_t p)
{
	sendByte(CMD_SETCUR + p);
	sendChar(c);
}

void printStringAt(const char *s, uint8_t p)
{
	sendByte(CMD_SETCUR + p);
	sendString(s);
}

void setCustomChar(uint8_t n, uint8_t* data)
{
	sendByte(CMD_SETCUCH);
	sendByte(n);

	for(int i=0; i<5; i++)
		sendByte(data[i]);
}

void sendChar(char c)
{
	SPI.transfer(c);
	delay(1);
}

void sendByte(uint8_t b)
{
	SPI.transfer(b);
	delay(1);
}

void sendString(const char *s)
{
	for(int i=0; i<strlen(s); i++){
		SPI.transfer(s[i]);
		delay(1);
	}
}
