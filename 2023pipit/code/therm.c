/** Example of using driver in therm.lus from C code */

/** The driver generates C code that defines a state type and a return type: */

typedef struct driver_state {
  // ...
} driver_state;

typedef struct driver_return {
  TCMD cmd;
  bool temp_read_success;
  bool temp_good;
  int  temp;
} driver_return;

/** And reset and step functions. */
void driver_reset(driver_state *state);
/** The step function performs a single step of the transition system, given input `rsp` and storing the result in `output` */
void driver_step(driver_state *state, driver_return *output, TRSP rsp);

/** Our driver will call the step function and interface with the hardware.
  It's conceptually just a big while loop, but in practice we'd probably split
  it out to a timer function that's run every 100ms or so because we'd have
  other work to do in the meantime.
 */

/** write one byte on i2c bus */
bool i2c_write1(char address, char register, char value);
/** read any number of bytes on i2c bus, returning number of bytes actually read */
int i2c_read(char address, char register, int nbytes, char *output);

void driver_c_loop() {
  driver_state state;
  driver_reset(&state);
  TRSP last_rsp = { false, 0, 0 };
  while (true) {
    driver_return r;
    driver_step(&state, &r, last_rsp);

    switch (r.cmd) {
      case CMD_RESET:
        last_rsp.cmd_ok = i2c_write1(I2C_TEMP, I2C_REG_RESET, 1);
        break;
      case CMD_SET_INT_ENABLE:
        last_rsp.cmd_ok = i2c_write1(I2C_TEMP, I2C_REG_INT, 1);
        break;
      case CMD_SET_INT_DISABLE:
        last_rsp.cmd_ok = i2c_write1(I2C_TEMP, I2C_REG_INT, 0);
        break;
      case CMD_READ:
        char bytes[2];
        int nbytes = i2c_read(I2C_TEMP, I2C_REG_TEMP, 2, &bytes);
        last_rsp.cmd_ok = (nbytes == 2);
        last_rsp.temp  = bytes[0];
        last_rsp.fresh = bytes[1];
        break;

      case CMD_NONE:
    }
  }
}
