       #include <stdio.h>
       #include <unistd.h>
       #include <jsw.h>

       int main(int argc, char *argv[])
       {
               int i, status;
               char *device = "/dev/js0";
               char *calib = ".joystick";
               js_data_struct jsd;

               if(argc < 3)
               {
                   printf(
       "Usage: basic <joystick_device> <calibation_file>\n"
                   );
                   return(0);
               }

               device = argv[1];
               calib = argv[2];

               /* Initialize joystick. */
               status = JSInit(
                   &jsd,
                   device,
                   calib,
                   JSFlagNonBlocking
               );

               if(status != JSSuccess)
               {
                   fprintf(
                       stderr,
       "Unable to open joystick, error code %i.\n",
                       status
                   );
                   JSClose(&jsd);
                   return(1);
               }

               while(1)
               {
                   if(JSUpdate(&jsd) == JSGotEvent)
                   {
                       printf("\r");

                       /* Print each axis position. */
                       for(i = 0; i < jsd.total_axises; i++)
                           printf(
                               "A%i:%.3f ",
                               i,
                               JSGetAxisCoeffNZ(&jsd, i)
                           );

                       printf(" ");

                       /* Print state of each button. */
                       for(i = 0; i < jsd.total_buttons; i++)
                           printf(
                               "B%i:%i ",
                               i,
                               JSGetButtonState(&jsd, i)
                           );

                       fflush(stdout);
                   }
                   usleep(16000);      /* Don't hog the cpu. */
               }

               /* Main while() loop never breaks, but if it
                * did, besure to close the joystick before
                * exiting.
                */
               JSClose(&jsd);

               return(0);
       }
       
