/*
 * Copyright (c) 2011-2012, Preston Skupinski <preston.skupinski@gmail.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

// this is a modified version

(function(window) {
// A collection of helper functions.
var bytesToString = function(bytes) {

  if( Object.prototype.toString.call(bytes) === '[object Array]' )  {
      var sReturn = "";
      var number;
      for(var i = bytes.length-1; i >= 0; i--) {
          number = Number(bytes[i]);
          sReturn += (number < 16 ? "0" : "")
                  + number.toString(16).toUpperCase();
      }
      return sReturn;
  } else {
      number = Number(bytes);
      return (number < 16 ? "0" : "")
             + number.toString(16).toUpperCase();
  }
}

var cpu_lib = {
  r: {
    p: {
      Set_p: function(value) {
        this.bytes_required = 2;
        this.execute = function(cpu, bytes) {
          // TODO: Figure out exactly how behavior differs in emulation mode.
          cpu.cycle_count+=3;

          var flags = bytes[0].toString(2),
          ops = { 0: function() { cpu.p.n = value; },
                  1: function() { cpu.p.v = value; },
                  2: function() { cpu.p.m = value; },
                  3: function() { cpu.p.x = value; },
                  4: function() { cpu.p.d = value; },
                  5: function() { cpu.p.i = value; },
                  6: function() { cpu.p.z = value; },
                  7: function() { cpu.p.c = value; }};

          // Sometimes it cuts off zeros before hand, so add those zeros back.
          while(flags.length<8) {
            flags = '0' + flags;
          }

          for(var i = 0; i < 8; i++) {
            if(flags.charAt(i)==='1') {
              ops[i]();
            }
          }
        }
      },
      Flag_set: function(flag, value) {
        this.bytes_required = 1;
        this.execute = function(cpu) {
          cpu.cycle_count+=2;
          cpu.instruction_details = "flag " + flag + " set to " + value;
          //cpu.instruction_translated = true;
          cpu.p[flag] = value;
        };
      },
      check_z: function(cpu, val) {
        if(val===0) {
          cpu.p.z = 1;
        } else {
          cpu.p.z = 0;
        }
      }
    }
  },
  branch: {
    Branch: function(flag, branch_on) {
      var always_branch = typeof flag === 'undefined';

      this.bytes_required = 2;

      this.execute = function(cpu, bytes) {
        cpu.cycle_count+=3;
        if(cpu.p.e)
          cpu.cycle_count++;

        if(always_branch||(cpu.p[flag]==branch_on)) {
          if(!always_branch)
            cpu.cycle_count++;

          // Handle single byte two's complement numbers as the branch argument.
          if(bytes[0]<=127) {
            cpu.r.pc+=bytes[0];
            cpu.r.pc&=0xffff;
          } else {
            cpu.r.pc-=256-bytes[0];
            if(cpu.r.pc<0)
              cpu.r.pc+=0xffff;
          }
        }
      }
    }
  },
  inc: {
    INC_register: function(register, flag, value) {
      if(typeof value === 'undefined')
        value = 1;

      this.bytes_required = 1;

      this.toString = function() {
        return (value == 1 ? "IN" : "DE") + " reg" + register + " flg" + flag;
      };

      this.execute = function(cpu) {
        cpu.cycle_count+=2;

        cpu.r[register]+=value;

        if(cpu.p.e||cpu.p[flag]) {
          cpu.r[register] &= 0xff;
          cpu.p.n = cpu.r[register] >> 7;
        } else {
          cpu.r[register] &= 0xffff;
          cpu.p.n = cpu.r[register] >> 15;
        }

        cpu_lib.r.p.check_z(cpu, cpu.r[register]);
      };
    },
    INC_memory: function(value) {
      if(typeof value === 'undefined')
        value = 1;

      this.execute = function(cpu, bytes, extra) {
        cpu.cycle_count+=4;
        if(cpu.p.e||cpu.p.m) {
          var temp = (bytes[0] + value) & 0xff;
          cpu.p.n = temp >> 7;
          cpu.mmu.store_byte(extra.memory_location, temp);
        } else {
          cpu.cycle_count+=2;
          var temp = (((bytes[1]<<8)|bytes[0]) + value) & 0xffff;
          cpu.p.n = temp >> 15;
          cpu.mmu.store_word(extra.memory_location, temp);
        }
        cpu_lib.r.p.check_z(cpu, temp);
      }
    }
  },
  addressing: {
    Direct_page: function(instruction) {
      this.bytes_required = 2;

      this.execute = function(cpu, bytes) {
        cpu.cycle_count++;
        cpu.instruction_translate = instruction.toString() + " $" + bytesToString(bytes);
        cpu.instruction_translated = true;
        cpu.instruction_history += " " + instruction.toString() + " $" + bytesToString(bytes);

        if((cpu.r.d&0xff)!==0)
          cpu.cycle_count++;

        var memory_location = bytes[0] + cpu.r.d;
        if(cpu.p.e||cpu.p.m) {
          instruction.execute(cpu, [cpu.mmu.read_byte(memory_location)],
                              {memory_location: memory_location});
        } else {
          instruction.execute(cpu, [cpu.mmu.read_byte(memory_location),
                                    cpu.mmu.read_byte(memory_location+1)],
                              {memory_location: memory_location});
        }
        cpu.instruction_details += "<br />Direct Page addressing";
        cpu.instruction_translated = false;
      };
    },
    Direct_page_indexed_x_indirect: function(instruction) {
      this.bytes_required = 2;

      this.execute = function(cpu, bytes) {
        cpu.cycle_count+=4;

        if((cpu.r.d&0xff)!==0)
          cpu.cycle_count++;

        if(cpu.p.e) {
          var memory_location = (bytes[0] + cpu.r.x) & 0xff,
              low_byte_loc = cpu.mmu.read_byte_long(memory_location+cpu.r.d,0),
              high_byte_read_loc = ((memory_location+1)&0xff)+cpu.r.d,
              high_byte_loc = cpu.mmu.read_byte_long(high_byte_read_loc, 0),
              address = (high_byte_loc<<8) | low_byte_loc;
          instruction.execute(cpu, [cpu.mmu.read_byte(address)],
                              { memory_location: address });
        } else if(cpu.p.m) {
          var memory_location = bytes[0] + cpu.r.d + cpu.r.x,
              low_byte_loc = cpu.mmu.read_byte(memory_location),
              high_byte_loc = cpu.mmu.read_byte(memory_location+1),
              address = (high_byte_loc<<8)|low_byte_loc;
          instruction.execute(cpu, [cpu.mmu.read_byte(address)],
                              { memory_location: address });
        } else {
          var memory_location = bytes[0] + cpu.r.d + cpu.r.x,
              absolute_location = cpu.mmu.read_word(memory_location),
              low_byte = cpu.mmu.read_byte(absolute_location),
              high_byte;
          absolute_location++;
          if(absolute_location&0x10000) {
            high_byte = cpu.mmu.read_byte_long(absolute_location, cpu.r.dbr+1);
          } else {
            high_byte = cpu.mmu.read_byte(absolute_location);
          }
          instruction.execute(cpu, [low_byte, high_byte],
                              { memory_location: absolute_location-1 });
        }
      };
    },
    Direct_page_indirect: function(instruction) {
      this.bytes_required = 2;

      this.execute = function(cpu, bytes) {
        cpu.cycle_count+=3;

        if((cpu.r.d&0xff)!==0)
          cpu.cycle_count++;

        var memory_location = bytes[0] + cpu.r.d,
            absolute_location = cpu.mmu.read_word(memory_location);
        if(cpu.p.e||cpu.p.m) {
          instruction.execute(cpu, [cpu.mmu.read_byte(absolute_location)],
                              { memory_location: absolute_location });
        } else {
          instruction.execute(cpu, [cpu.mmu.read_byte(absolute_location),
                                    cpu.mmu.read_byte(absolute_location+1)],
                              { memory_location: absolute_location });
        }
      };
    },
    Direct_page_indirect_indexed_y: function(instruction) {
      this.bytes_required = 2;

      this.execute = function(cpu, bytes) {
        cpu.cycle_count+=3;

        if((cpu.r.d&0xff)!==0)
          cpu.cycle_count++;

        var memory_location = bytes[0] + cpu.r.d,
            original_location = cpu.mmu.read_word(memory_location),
            absolute_location = original_location + cpu.r.y;

        if((original_location&0xff00)!==(absolute_location&0xff00))
          cpu.cycle_count++;

        if(cpu.p.e||cpu.p.m) {
          instruction.execute(cpu, [cpu.mmu.read_byte(absolute_location)],
                              { memory_location: absolute_location });
        } else {
          instruction.execute(cpu, [cpu.mmu.read_byte(absolute_location),
                                    cpu.mmu.read_byte(absolute_location+1)],
                              { memory_location: absolute_location });
        }
      };
    },
    Direct_page_indirect_long: function(instruction) {
      this.bytes_required = 2;

      this.execute = function(cpu, bytes) {
        cpu.cycle_count+=4;

        if((cpu.r.d&0xff)!==0)
          cpu.cycle_count++;

        var memory_location = bytes[0] + cpu.r.d,
            bank_byte = cpu.mmu.read_byte(memory_location+2),
            absolute_location = cpu.mmu.read_word(memory_location);
        if(cpu.p.e||cpu.p.m) {
          instruction.execute(cpu, [cpu.mmu.read_byte_long(absolute_location,
                                                           bank_byte)],
                              { memory_location: absolute_location });
        } else {
          instruction.execute(cpu, [cpu.mmu.read_byte_long(absolute_location,
                                                           bank_byte),
                                    cpu.mmu.read_byte_long(absolute_location+1,
                                                           bank_byte)],
                              { memory_location: absolute_location });
        }
      };
    },
    Direct_page_indirect_long_indexed_y: function(instruction) {
      this.bytes_required = 2;

      this.execute = function(cpu, bytes) {
        cpu.cycle_count+=4;

        if((cpu.r.d&0xff)!==0)
          cpu.cycle_count++;

        var memory_location = bytes[0] + cpu.r.d,
            bank_byte = cpu.mmu.read_byte(memory_location+2),
            absolute_location = cpu.mmu.read_word(memory_location) + cpu.r.y;
        if(absolute_location >> 16) {
          absolute_location &= 0xffff;
          bank_byte++;
        }

        if(cpu.p.e||cpu.p.m) {
          instruction.execute(cpu, [cpu.mmu.read_byte_long(absolute_location,
                                                           bank_byte)],
                              { memory_location: absolute_location });
        } else {
          var low_byte = cpu.mmu.read_byte_long(absolute_location, bank_byte);
          absolute_location++;
          if(absolute_location >> 16) {
            absolute_location &= 0xffff;
            bank_byte++;
          }
          var high_byte = cpu.mmu.read_byte_long(absolute_location, bank_byte);
          instruction.execute(cpu, [low_byte, high_byte],
                              { memory_location: absolute_location-1 });
        }
      };
    },
    Absolute: function(instruction) {
      this.bytes_required = 3;

      this.execute = function(cpu, bytes) {
        cpu.cycle_count+=2;

        var memory_location = (bytes[1]<<8)|bytes[0];
        if(cpu.p.e||cpu.p.m) {
          instruction.execute(cpu, [cpu.mmu.read_byte(memory_location)],
                              { memory_location: memory_location });
        } else {
          instruction.execute(cpu, [cpu.mmu.read_byte(memory_location),
                                    cpu.mmu.read_byte(memory_location+1)],
                              { memory_location: memory_location });
        }
      };
    },
    Absolute_long: function(instruction) {
      this.bytes_required = 4;

      this.execute = function(cpu, bytes) {
        cpu.cycle_count+=3;

        var memory_location = (bytes[1]<<8)|bytes[0];
        if(cpu.p.e||cpu.p.m) {
          instruction.execute(cpu, [cpu.mmu.read_byte_long(memory_location,
                                                           bytes[2])],
                              { memory_location: memory_location });
        } else {
          instruction.execute(cpu, [cpu.mmu.read_byte_long(memory_location,
                                                           bytes[2]),
                                    cpu.mmu.read_byte_long(memory_location+1,
                                                           bytes[2])],
                              { memory_location: memory_location });
        }
      };
    },
    Absolute_long_indexed_x: function(instruction) {
      this.bytes_required = 4;

      this.execute = function(cpu, bytes) {
        cpu.cycle_count+=3;

        var memory_location = ((bytes[1]<<8)|bytes[0]) + cpu.r.x;
        if(memory_location & 0x10000) {
          bytes[2]++;
          memory_location &= 0xffff;
        }

        if(cpu.p.e||cpu.p.m) {
          instruction.execute(cpu, [cpu.mmu.read_byte_long(memory_location,
                                                           bytes[2])],
                              { memory_location: memory_location });
        } else {
          var low_byte = cpu.mmu.read_byte_long(memory_location, bytes[2]);
          memory_location++;
          if(memory_location & 0x10000) {
            bytes[2]++;
            memory_location &= 0xffff;
          }
          var high_byte = cpu.mmu.read_byte_long(memory_location, bytes[2]);
          instruction.execute(cpu, [low_byte, high_byte],
                              { memory_location: memory_location - 1 });
        }
      };
    },
    Absolute_indexed_x: function(absolute_instruction, other) {
      this.bytes_required = 3;

      this.execute = function(cpu, bytes) {
        var memory_location;

        if(other) {
          cpu.cycle_count++;
          memory_location = ((bytes[1]<<8)|bytes[0])+cpu.r.x;
        } else {
          var initial_location = (bytes[1]<<8)|bytes[0];
          memory_location = initial_location+cpu.r.x;
          if((memory_location&0xff00)!==(initial_location&0xff00))
            cpu.cycle_count++;
        }
        absolute_instruction.execute(cpu, [memory_location&0xff,
                                           memory_location>>8]);
      };
    },
    Absolute_indexed_y: function(absolute_instruction) {
      this.bytes_required = 3;

      this.execute = function(cpu, bytes) {
        var initial_location = (bytes[1]<<8)|bytes[0],
            memory_location = initial_location+cpu.r.y;

        if((memory_location&0xff00)!==(initial_location&0xff00))
          cpu.cycle_count++;

        absolute_instruction.execute(cpu, [memory_location&0xff,
                                           memory_location>>8]);
      };
    },
    Direct_page_indexed: function(direct_page_instruction, register) {
      this.bytes_required = 2;

      this.execute = function(cpu, bytes) {
        cpu.cycle_count++;

        direct_page_instruction.execute(cpu, [bytes[0]+cpu.r[register]]);
      };
    },
    Stack_relative: function(instruction) {
      this.bytes_required = 2;

      this.execute = function(cpu, bytes) {
        cpu.cycle_count+=2;

        var memory_location = cpu.r.s + bytes[0];
        if(cpu.p.e) {
          memory_location = 0x100 | (memory_location & 0xff);
        }

        if(cpu.p.e || cpu.p.m) {
          instruction.execute(cpu, [cpu.mmu.read_byte(memory_location)]);
        } else {
          instruction.execute(cpu, [cpu.mmu.read_byte(memory_location),
                                    cpu.mmu.read_byte(memory_location+1)]);
        }
      };
    },
    Stack_relative_indirect_indexed_y: function(instruction) {
      this.bytes_required = 2;

      this.execute = function(cpu, bytes) {
        cpu.cycle_count+=5;

        if(cpu.p.e) {
          var location_loc = 0x100 | ((cpu.r.s + bytes[0]) & 0xff),
              low_byte =  cpu.mmu.read_byte(location_loc),
              high_byte;
          if(location_loc===0x1ff) {
            high_byte = cpu.mmu.read_byte(0x100);
          } else {
            high_byte = cpu.mmu.read_byte(location_loc+1);
          }
          var absolute_location = ((high_byte<<8)|low_byte)+cpu.r.y,
              b;
          if(absolute_location>=0x10000) {
            b = cpu.mmu.read_byte_long(absolute_location, cpu.r.dbr+1);
          } else {
            b = cpu.mmu.read_byte(absolute_location);
          }
          instruction.execute(cpu, [b]);
        } else {
          var location_loc = (cpu.r.s + bytes[0]) & 0xffff,
              absolute_location = cpu.mmu.read_word(location_loc) + cpu.r.y;
          if(cpu.p.m) {
            var b;
            if(absolute_location>=0x10000) {
              b = cpu.mmu.read_byte_long(absolute_location, cpu.r.dbr+1);
            } else {
              b = cpu.mmu.read_byte(absolute_location);
            }
            instruction.execute(cpu, [b]);
          } else {
            var low_byte, high_byte;
            if(absolute_location>=0x10000) {
              absolute_location &= 0xffff;
              low_byte = cpu.mmu.read_byte_long(absolute_location, cpu.r.dbr+1);
              high_byte = cpu.mmu.read_byte_long(absolute_location+1,
                                                 cpu.r.dbr+1);
            } else {
              low_byte = cpu.mmu.read_byte(absolute_location);
              if(absolute_location===0xffff) {
                 high_byte = cpu.mmu.read_byte_long(0, cpu.r.dbr+1);
              } else {
                 high_byte = cpu.mmu.read_byte(absolute_location);
              }
            }
            instruction.execute(cpu, [low_byte, high_byte]);
          }
        }
      }
    }
  }
};

var STP = {
  toString: function() { return "STP" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=3;

    cpu.stopped = true;
  }
};

var WAI = {
  toString: function() { return "WAI" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=3;

    cpu.waiting = true;
  }
};

var WDM = {
  toString: function() { return "WDM" },
  bytes_required:2,

  execute:function() {}
};

var TXS = {
  toString: function() { return "TXS" },
  bytes_required:1,
  execute:function(cpu) {
    cpu.cycle_count+=2;
    cpu.r.s = cpu.r.x;
    if(cpu.p.e||cpu.p.x) {
      cpu.p.n = cpu.r.s >> 7;
    } else {
      cpu.p.n = cpu.r.s >> 15;
    }
    cpu_lib.r.p.check_z(cpu, cpu.r.s);
  }
};

var TSX = {
  toString: function() { return "TSX" },
  bytes_required:1,
  execute:function(cpu) {
    cpu.cycle_count+=2;
    if(cpu.p.e||cpu.p.x) {
      cpu.r.x = cpu.r.s & 0xff;
      cpu.p.n = cpu.r.x >> 7;
    } else {
      cpu.r.x = cpu.r.s;
      cpu.p.n = cpu.r.x >> 15;
    }
    cpu_lib.r.p.check_z(cpu, cpu.r.x);
  }
};

var TRB_absolute = {
  toString: function() { return "TRB" },
  bytes_required:3,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=6;

    var memory_location = (bytes[1]<<8)|bytes[0],
        data;
    if(cpu.p.e||cpu.p.m) {
      data = cpu.mmu.read_byte(memory_location);
      cpu_lib.r.p.check_z(cpu, data & cpu.r.a);
      cpu.mmu.store_byte(memory_location, (~cpu.r.a & data));
    } else {
      cpu.cycle_count+=2;

      data = cpu.mmu.read_word(memory_location);
      cpu_lib.r.p.check_z(cpu, data & cpu.r.a);
      data &= ~cpu.r.a;
      cpu.mmu.store_word(memory_location, data);
    }
  }
};

var TRB_direct_page = {
  toString: function() { return "TRB" },
  bytes_required:2,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=5;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0] + cpu.r.d,
        data;
    if(cpu.p.e||cpu.p.m) {
      data = cpu.mmu.read_byte(memory_location);
      cpu_lib.r.p.check_z(cpu, data & cpu.r.a);
      cpu.mmu.store_byte(memory_location, (~cpu.r.a & data));
    } else {
      cpu.cycle_count+=2;

      data = cpu.mmu.read_word(memory_location);
      cpu_lib.r.p.check_z(cpu, data & cpu.r.a);
      data &= ~cpu.r.a;
      cpu.mmu.store_word(memory_location, data);
    }
  }
};

var TSB_absolute = {
  toString: function() { return "TSB" },
  bytes_required:3,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=6;

    var memory_location = (bytes[1]<<8)|bytes[0],
        data;
    if(cpu.p.e||cpu.p.m) {
      data = cpu.mmu.read_byte(memory_location);
      cpu_lib.r.p.check_z(cpu, data & cpu.r.a);
      cpu.mmu.store_byte(memory_location, (cpu.r.a | data));
    } else {
      cpu.cycle_count+=2;

      data = cpu.mmu.read_word(memory_location);
      cpu_lib.r.p.check_z(cpu, data & cpu.r.a);
      data |= cpu.r.a;
      cpu.mmu.store_word(memory_location, data);
    }
  }
};

var TSB_direct_page = {
  toString: function() { return "TSB" },
  bytes_required:2,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=5;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0] + cpu.r.d,
        data;
    if(cpu.p.e||cpu.p.m) {
      data = cpu.mmu.read_byte(memory_location);
      cpu_lib.r.p.check_z(cpu, data & cpu.r.a);
      cpu.mmu.store_byte(memory_location, (cpu.r.a | data));
    } else {
      cpu.cycle_count+=2;

      data = cpu.mmu.read_word(memory_location);
      cpu_lib.r.p.check_z(cpu, data & cpu.r.a);
      data |= cpu.r.a;
      cpu.mmu.store_word(memory_location, data);
    }
  }
};

var BIT_const = {
  toString: function() { return "BIT" },
  bytes_required:function(cpu) {
    if(cpu.p.e||cpu.p.m)
      return 2;
    else
      return 3;
  },
  execute:function(cpu, bytes) {
    cpu.instruction_details += "<br />BIT test between value and accumulator";
    if(!cpu.instruction_translated) {
      cpu.instruction_details += " (const)";
      cpu.instruction_translate = this.toString() + " #" + bytesToString(bytes);
      cpu.instruction_history += " " + this.toString() + " #" + bytesToString(bytes);
    }
    cpu.cycle_count+=2;

    var and_result;
    if(cpu.p.e||cpu.p.m) {
      cpu.p.n = bytes[0] >> 7;
      cpu.p.v = (bytes[0] >> 6) & 0x1;
      and_result = cpu.r.a & bytes[0];
    } else {
      cpu.cycle_count++;
      cpu.p.n = bytes[1] >> 7;
      cpu.p.v = (bytes[1] >> 6) & 0x1;
      and_result = cpu.r.a & ((bytes[1]<<8)|bytes[0]);
    }

    cpu_lib.r.p.check_z(cpu, and_result);
  }
};

var BIT_absolute = new cpu_lib.addressing.Absolute(BIT_const);

var BIT_direct_page = new cpu_lib.addressing.Direct_page(BIT_const);

var BIT_direct_page_indexed_x =
  new cpu_lib.addressing.Direct_page_indexed(BIT_direct_page, 'x');

var BIT_absolute_indexed_x =
  new cpu_lib.addressing.Absolute_indexed_x(BIT_absolute);

var COP = {
  toString: function() { return "COP" },
  bytes_required:2,

  execute:function(cpu) {
    cpu.cycle_count+=7;

    if(cpu.p.e===0)
      cpu.cycle_count++;

    cpu.interrupt = cpu.INTERRUPT.COP;
  }
};

var BRK = {
  toString: function() { return "BRK" },
  bytes_required:2,

  execute:function(cpu) {
    cpu.cycle_count+=7;

    if(cpu.p.e===0)
      cpu.cycle_count++;

    cpu.interrupt = cpu.INTERRUPT.BRK;
    if(cpu.p.e)
      cpu.p.m = 1;
  }
};

var RTI = {
  toString: function() { return "RTI" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=6;

    if(cpu.p.e===0)
      cpu.cycle_count++;

    var p_byte = cpu.mmu.pull_byte(),
        pc_low = cpu.mmu.pull_byte(),
        pc_high = cpu.mmu.pull_byte();
    cpu.r.pc = (pc_high<<8)|pc_low;

    cpu.p.c = p_byte & 0x01;
    cpu.p.z = (p_byte & 0x02) >> 1;
    cpu.p.i = (p_byte & 0x04) >> 2;
    cpu.p.d = (p_byte & 0x08) >> 3;
    cpu.p.x = (p_byte & 0x10) >> 4;
    cpu.p.m = (p_byte & 0x20) >> 5;
    cpu.p.v = (p_byte & 0x40) >> 6;
    cpu.p.n = p_byte >> 7;

    if(!cpu.p.e) {
      cpu.r.k = cpu.mmu.pull_byte();
    }
  }
};

// MVN is a really weird instruction, until the accumulator underflows MVN
// will keep decrementing the program counter to have it continue to execute.
var MVN = {
  toString: function() { return "MVN" },
  bytes_required:3,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=7;

    // TODO: One piece of reference material I've read claims that this
    // operation should always work with a 16-bit accumulator even if in
    // emulation mode or the m bit is set to 1, in those cases it claims that
    // you should concatenate the B "hidden" register with A.  I'm going to
    // need to test this claim out somehow.
    var b = cpu.mmu.read_byte_long(cpu.r.x,bytes[1]);
    cpu.r.dbr = bytes[0];
    cpu.mmu.store_byte(cpu.r.y, b);
    cpu.r.x++;
    cpu.r.y++;
    if(cpu.p.e||cpu.p.x) {
      cpu.r.x &= 0x00ff;
      cpu.r.y &= 0x00ff;
    } else {
      cpu.r.x &= 0xffff;
      cpu.r.y &= 0xffff;
    }

    if(cpu.r.a!==0) {
      cpu.r.a--;
      cpu.r.pc-=3;
    } else {
      if(cpu.p.e||cpu.p.m)
        cpu.r.a = 0xff;
      else
        cpu.r.a = 0xffff;
    }
  }
};

// MVP is a really weird instruction, until the accumulator reaches $FFFF MVP
// will keep decrementing the program counter to have it continue to execute.
var MVP = {
  toString: function() { return "MVP" },
  bytes_required:3,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=7;

    // TODO: One piece of reference material I've read claims that this
    // operation should always work with a 16-bit accumulator even if in
    // emulation mode or the m bit is set to 1, in those cases it claims that
    // you should concatenate the B "hidden" register with A.  I'm going to
    // need to test this claim out somehow.
    var b = cpu.mmu.read_byte_long(cpu.r.x,bytes[1]);
    cpu.r.dbr = bytes[0];
    cpu.mmu.store_byte(cpu.r.y,b);

    var index_register_wrap;
    if(cpu.p.e||cpu.p.x) {
      index_register_wrap = 0xff;
    } else {
      index_register_wrap = 0xffff;
    }

    if(cpu.r.y===index_register_wrap) {
      cpu.r.y = 0;
    } else {
      cpu.r.y--;
    }

    if(cpu.r.x===index_register_wrap) {
      cpu.r.x = 0;
    } else {
      cpu.r.x--;
    }

    if(cpu.r.a!==0) {
      cpu.r.pc-=3;
      cpu.r.a--;
    } else {
      if(cpu.p.e||cpu.p.m)
        cpu.r.a = 0xff;
      else
        cpu.r.a = 0xffff;
    }
  }
};

var JSL = {
  toString: function() { return "JSL" },
  bytes_required:4,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=8;

    var memory_location = cpu.r.pc - 1;
    cpu.mmu.push_byte(cpu.r.k);
    cpu.mmu.push_byte(memory_location>>8);
    cpu.mmu.push_byte(memory_location&0x00ff);
    cpu.r.k = bytes[2];
    cpu.r.pc = (bytes[1]<<8)|bytes[0];
  }
};

var RTL = {
  toString: function() { return "RTL" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=6;

    var low_byte = cpu.mmu.pull_byte(),
        high_byte = cpu.mmu.pull_byte();
    cpu.r.k = cpu.mmu.pull_byte();
    cpu.r.pc = ((high_byte<<8)|low_byte) + 1;
  }
};

var JSR = {
  toString: function() { return "JSR" },
  bytes_required:3,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=6;

    var memory_location = cpu.r.pc - 1;
    cpu.mmu.push_byte(memory_location>>8);
    cpu.mmu.push_byte(memory_location&0xff);
    cpu.r.pc = (bytes[1]<<8)|bytes[0];
  }
};

var JSR_absolute_indexed_x_indirect = {
  toString: function() { return "JSR" },
  bytes_required:3,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=8;

    var memory_location = ((bytes[1]<<8)|bytes[0])+cpu.r.x,
        bank = cpu.r.k;
    if(memory_location&0x10000) {
       bank++;
    }
    memory_location &= 0xffff;
    var indirect_location = cpu.mmu.read_word_long(memory_location, bank),
        low_byte = cpu.mmu.read_byte(indirect_location);
    bank = cpu.r.k;
    if(indirect_location===0xffff) {
      indirect_location = 0;
      bank++;
    } else {
      indirect_location++;
    }
    var high_byte = cpu.mmu.read_byte_long(indirect_location, bank);
    JSR.execute(cpu, [low_byte, high_byte]);
  }
};

var RTS = {
  toString: function() { return "RTS" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=6;

    var low_byte = cpu.mmu.pull_byte(),
        high_byte = cpu.mmu.pull_byte();
    cpu.r.pc = ((high_byte<<8)|low_byte) + 1;
  }
};

var PER = {
  toString: function() { return "PER" },
  bytes_required:3,

  execute:function(cpu,bytes) {
    cpu.cycle_count+=6;

    var address = ((bytes[1]<<8)|bytes[0]) + cpu.r.pc;
    cpu.mmu.push_byte(address>>8);
    cpu.mmu.push_byte(address&0xff);
  }
};

var PHK = {
  toString: function() { return "PHK" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=3;

    cpu.mmu.push_byte(cpu.r.k);
  }
};

var PHD = {
  toString: function() { return "PHD" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=4;

    cpu.mmu.push_byte(cpu.r.d>>8);
    cpu.mmu.push_byte(cpu.r.d&0xff);
  }
};

var PLD = {
  toString: function() { return "PLD" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=5;

    var low_byte = cpu.mmu.pull_byte(),
        high_byte = cpu.mmu.pull_byte();
    cpu.r.d = (high_byte<<8)|low_byte;

    cpu.p.n = cpu.r.d >> 15;

    cpu_lib.r.p.check_z(cpu, cpu.r.d);
  }
};

var PHB = {
  toString: function() { return "PHB" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=3;
    cpu.mmu.push_byte(cpu.r.dbr);
  }
};

var PLB = {
  toString: function() { return "PLB" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=4;

    cpu.r.dbr = cpu.mmu.pull_byte();
    cpu.p.n = cpu.r.dbr >> 7;
    cpu_lib.r.p.check_z(cpu, cpu.r.dbr);
  }
};

var PEA = {
  toString: function() { return "PEA" },
  bytes_required:3,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=5;

    cpu.mmu.push_byte(bytes[1]);
    cpu.mmu.push_byte(bytes[0]);
  }
};

var PEI = {
  toString: function() { return "PEI" },
  bytes_required:2,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=6;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0]+cpu.r.d;
    cpu.mmu.push_byte(cpu.mmu.read_byte(memory_location+1));
    cpu.mmu.push_byte(cpu.mmu.read_byte(memory_location));
  }
};

var PHP = {
  toString: function() { return "PHP" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=3;

    var p_byte = (cpu.p.n<<7)|(cpu.p.v<<6)|(cpu.p.m<<5)|(cpu.p.x<<4)|
                 (cpu.p.d<<3)|(cpu.p.i<<2)|(cpu.p.z<<1)|cpu.p.c;
    cpu.mmu.push_byte(p_byte);
  }
};

var PLP = {
  toString: function() { return "PLP" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=4;

    var p_byte = cpu.mmu.pull_byte();
    cpu.p.c = p_byte & 0x01;
    cpu.p.z = (p_byte & 0x02) >> 1;
    cpu.p.i = (p_byte & 0x04) >> 2;
    cpu.p.d = (p_byte & 0x08) >> 3;
    cpu.p.x = (p_byte & 0x10) >> 4;
    cpu.p.m = (p_byte & 0x20) >> 5;
    cpu.p.v = (p_byte & 0x40) >> 6;
    cpu.p.n = p_byte >> 7;
  }
};

var PHX = {
  toString: function() { return "PHX" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=3;

    if(cpu.p.e||cpu.p.x) {
      cpu.mmu.push_byte(cpu.r.x);
    } else {
      cpu.cycle_count++;

      cpu.mmu.push_byte(cpu.r.x>>8);
      cpu.mmu.push_byte(cpu.r.x&0xff);
    }
  }
};

var PLX = {
  toString: function() { return "PLX" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=4;

    if(cpu.p.e||cpu.p.x) {
      cpu.r.x = cpu.mmu.pull_byte();
      cpu.p.n = cpu.r.x >> 7;
    } else {
      cpu.cycle_count++;
      var low_byte = cpu.mmu.pull_byte(),
          high_byte = cpu.mmu.pull_byte();
      cpu.r.x = (high_byte<<8)|low_byte;
      cpu.p.n = cpu.r.x >> 15;
    }

    cpu_lib.r.p.check_z(cpu, cpu.r.x);
  }
};

var PHY = {
  toString: function() { return "PHY" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=3;

    if(cpu.p.e||cpu.p.x) {
      cpu.mmu.push_byte(cpu.r.y);
    } else {
      cpu.cycle_count++;
      cpu.mmu.push_byte(cpu.r.y>>8);
      cpu.mmu.push_byte(cpu.r.y&0xff);
    }
  }
};

var PLY = {
  toString: function() { return "PLY" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=4;

    if(cpu.p.e||cpu.p.x) {
      cpu.r.y = cpu.mmu.pull_byte();
      cpu.p.n = cpu.r.y >> 7;
    } else {
      cpu.cycle_count++;
      var low_byte = cpu.mmu.pull_byte(),
          high_byte = cpu.mmu.pull_byte();
      cpu.r.y = (high_byte<<8)|low_byte;
      cpu.p.n = cpu.r.y >> 15;
    }

    cpu_lib.r.p.check_z(cpu, cpu.r.y);
  }
};

var PHA = {
  toString: function() { return "PHA" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=3;

    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.push_byte(cpu.r.a);
    } else {
      cpu.cycle_count++;

      cpu.mmu.push_byte(cpu.r.a>>8);
      cpu.mmu.push_byte(cpu.r.a&0xff);
    }
  }
};

var PLA = {
  toString: function() { return "PLA" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=4;

    if(cpu.p.e||cpu.p.m) {
      cpu.r.a = cpu.mmu.pull_byte();
      cpu.p.n = cpu.r.a >> 7;
    } else {
      cpu.cycle_count++;

      var low_byte = cpu.mmu.pull_byte(),
          high_byte = cpu.mmu.pull_byte();
      cpu.r.a = (high_byte<<8)|low_byte;
      cpu.p.n = cpu.r.a >> 15;
    }

    cpu_lib.r.p.check_z(cpu, cpu.r.a);
  }
};

var ROR_accumulator = {
  toString: function() { return "ROR" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=2;

    var old_c = cpu.p.c;
    if(cpu.p.e||cpu.p.m) {
      cpu.p.c = cpu.r.a & 0x01;
      cpu.r.a = cpu.r.a >> 1;
      cpu.r.a &= 0x7f;
      cpu.r.a |= (old_c<<7);
      cpu.p.n = cpu.r.a >> 7;
    } else {
      cpu.p.c = cpu.r.a & 0x0001;
      cpu.r.a = cpu.r.a >> 1;
      cpu.r.a &= 0x7fff;
      cpu.r.a |= (old_c<<15);
      cpu.p.n = cpu.r.a >> 15;
    }

    cpu_lib.r.p.check_z(cpu, cpu.r.a);
  }
};

var ROR_absolute = {
  toString: function() { return "ROR" },
  bytes_required:3,

  execute:function(cpu,bytes) {
    cpu.cycle_count+=6;

    var memory_location = (bytes[1]<<8)|bytes[0],
        old_c = cpu.p.c,
        shiftee;
    if(cpu.p.e||cpu.p.m) {
      shiftee = cpu.mmu.read_byte(memory_location);
      cpu.p.c = shiftee & 0x01;
      shiftee = shiftee >> 1;
      shiftee &= 0x7f;
      shiftee |= (old_c<<7);
      cpu.p.n = shiftee >> 7;
      cpu.mmu.store_byte(memory_location, shiftee);
    } else {
      cpu.cycle_count+=2;

      shiftee = cpu.mmu.read_word(memory_location);
      cpu.p.c = shiftee & 0x0001;
      shiftee = shiftee >> 1;
      shiftee &= 0x7fff;
      shiftee |= (old_c<<15);
      cpu.p.n = shiftee >> 15;
      cpu.mmu.store_word(memory_location, shiftee);
    }

    cpu_lib.r.p.check_z(cpu, shiftee);
  }
};

var ROR_direct_page = {
  toString: function() { return "ROR" },
  bytes_required:2,

  execute:function(cpu,bytes) {
    cpu.cycle_count+=5;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0]+cpu.r.d,
        old_c = cpu.p.c,
        shiftee;
    if(cpu.p.e||cpu.p.m) {
      shiftee = cpu.mmu.read_byte(memory_location);
      cpu.p.c = shiftee & 0x01;
      shiftee = shiftee >> 1;
      shiftee &= 0x7f;
      shiftee |= (old_c<<7);
      cpu.p.n = shiftee >> 7;
      cpu.mmu.store_byte(memory_location, shiftee);
    } else {
      cpu.cycle_count+=2;

      shiftee = cpu.mmu.read_word(memory_location);
      cpu.p.c = shiftee & 0x0001;
      shiftee = shiftee >> 1;
      shiftee &= 0x7fff;
      shiftee |= (old_c<<15);
      cpu.p.n = shiftee >> 15;
      cpu.mmu.store_word(memory_location, shiftee);
    }

    cpu_lib.r.p.check_z(cpu, shiftee);
  }
};

var ROR_absolute_indexed_x =
  new cpu_lib.addressing.Absolute_indexed_x(ROR_absolute, true);

var ROR_direct_page_indexed_x =
  new cpu_lib.addressing.Direct_page_indexed(ROR_direct_page, 'x');

var ROL_accumulator = {
  toString: function() { return "ROL" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=2;

    var old_c = cpu.p.c;
    if(cpu.p.e||cpu.p.m) {
      cpu.p.c = cpu.r.a >> 7;
      cpu.r.a = cpu.r.a << 1;
      cpu.r.a &= 0xfe;
      cpu.r.a |= old_c;
      cpu.p.n = cpu.r.a >> 7;
    } else {
      cpu.p.c = cpu.r.a >> 15;
      cpu.r.a = cpu.r.a << 1;
      cpu.r.a &= 0xfffe;
      cpu.r.a |= old_c;
      cpu.p.n = cpu.r.a >> 15;
    }

    cpu_lib.r.p.check_z(cpu, cpu.r.a);
  }
};

var ROL_absolute = {
  toString: function() { return "ROL" },
  bytes_required:3,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=6;

    var memory_location = (bytes[1]<<8)|bytes[0],
        old_c = cpu.p.c,
        shiftee;
    if(cpu.p.e||cpu.p.m) {
      shiftee = cpu.mmu.read_byte(memory_location);
      cpu.p.c = shiftee >> 7;
      shiftee = shiftee << 1;
      shiftee &= 0xfe;
      shiftee |= old_c;
      cpu.p.n = shiftee >> 7;
      cpu.mmu.store_byte(memory_location, shiftee);
    } else {
      cpu.cycle_count+=2;

      shiftee = cpu.mmu.read_word(memory_location);
      cpu.p.c = shiftee >> 15;
      shiftee = shiftee << 1;
      shiftee &= 0xfffe;
      shiftee |= old_c;
      cpu.p.n = shiftee >> 15;
      cpu.mmu.store_word(memory_location, shiftee);
    }

    cpu_lib.r.p.check_z(cpu, shiftee);
  }
};

var ROL_direct_page = {
  toString: function() { return "ROL" },
  bytes_required:2,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=5;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0]+cpu.r.d,
        old_c = cpu.p.c,
        shiftee;
    if(cpu.p.e||cpu.p.m) {
      shiftee = cpu.mmu.read_byte(memory_location);
      cpu.p.c = shiftee >> 7;
      shiftee = shiftee << 1;
      shiftee &= 0xfe;
      shiftee |= old_c;
      cpu.p.n = shiftee >> 7;
      cpu.mmu.store_byte(memory_location, shiftee);
    } else {
      cpu.cycle_count+=2;

      shiftee = cpu.mmu.read_word(memory_location);
      cpu.p.c = shiftee >> 15;
      shiftee = shiftee << 1;
      shiftee &= 0xfffe;
      shiftee |= old_c;
      cpu.p.n = shiftee >> 15;
      cpu.mmu.store_word(memory_location, shiftee);
    }

    cpu_lib.r.p.check_z(cpu, shiftee);
  }
};

var ROL_absolute_indexed_x =
  new cpu_lib.addressing.Absolute_indexed_x(ROL_absolute, true);

var ROL_direct_page_indexed_x =
  new cpu_lib.addressing.Direct_page_indexed(ROL_direct_page, 'x');

var ASL_accumulator = {
  toString: function() { return "ASL" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=2;

    if(cpu.p.e||cpu.p.m) {
      cpu.p.c = cpu.r.a >> 7;
      cpu.r.a = cpu.r.a << 1;
      cpu.r.a &= 0xff;
      cpu.p.n = cpu.r.a >> 7;
    } else {
      cpu.p.c = cpu.r.a >> 15;
      cpu.r.a = cpu.r.a << 1;
      cpu.r.a &= 0xffff;
      cpu.p.n = cpu.r.a >> 15;
    }

    cpu_lib.r.p.check_z(cpu, cpu.r.a);
  }
};

var ASL_absolute = {
  toString: function() { return "ASL" },
  bytes_required:3,

  execute:function(cpu, bytes) {
    cpu.instruction_details += "<br />Shift Accumulator with value";
    if(!cpu.instruction_translated) {
      cpu.instruction_details += " (const)";
      cpu.instruction_translate = this.toString() + " #" + bytesToString(bytes);
      cpu.instruction_history += " " + this.toString() + " #" + bytesToString(bytes);
    }
    cpu.cycle_count+=6;

    var memory_location = (bytes[1]<<8)|bytes[0],
        shiftee;
    if(cpu.p.e||cpu.p.m) {
      shiftee = cpu.mmu.read_byte(memory_location);
      cpu.p.c = shiftee >> 7;
      shiftee = shiftee << 1;
      shiftee &= 0xff;
      cpu.p.n = shiftee >> 7;
      cpu.mmu.store_byte(memory_location, shiftee);
    } else {
      cpu.cycle_count+=2;
      shiftee = cpu.mmu.read_word(memory_location);
      cpu.p.c = shiftee >> 15;
      shiftee = shiftee << 1;
      shiftee &= 0xffff;
      cpu.p.n = shiftee >> 15;
      cpu.mmu.store_word(memory_location, shiftee);
    }

    cpu_lib.r.p.check_z(cpu, shiftee);
  }
};

var ASL_direct_page = {
  toString: function() { return "ASL" },
  bytes_required:2,

  execute:function(cpu,bytes) {
    cpu.instruction_details += "<br />Shift Accumulator with value";
    if(!cpu.instruction_translated) {
      cpu.instruction_details += "<br /> Direct Page addressing";
      cpu.instruction_translate = this.toString() + " $" + bytesToString(bytes);
      cpu.instruction_history += " " + this.toString() + " $" + bytesToString(bytes);
    }
    cpu.cycle_count+=5;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0]+cpu.r.d,
        shiftee;
    if(cpu.p.e||cpu.p.m) {
      shiftee = cpu.mmu.read_byte(memory_location);
      cpu.p.c = shiftee >> 7;
      shiftee = shiftee << 1;
      shiftee &= 0xff;
      cpu.p.n = shiftee >> 7;
      cpu.mmu.store_byte(memory_location, shiftee);
    } else {
      cpu.cycle_count+=2;

      shiftee = cpu.mmu.read_word(memory_location);
      cpu.p.c = shiftee >> 15;
      shiftee = shiftee << 1;
      shiftee &= 0xffff;
      cpu.p.n = shiftee >> 15;
      cpu.mmu.store_word(memory_location, shiftee);
    }

    cpu_lib.r.p.check_z(cpu, shiftee);
  }
};

var ASL_absolute_indexed_x =
  new cpu_lib.addressing.Absolute_indexed_x(ASL_absolute);

var ASL_direct_page_indexed_x =
  new cpu_lib.addressing.Direct_page_indexed(ASL_direct_page, 'x');

var LSR_accumulator= {
  toString: function() { return "LSR" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=2;

    cpu.p.c = cpu.r.a & 1;
    cpu.r.a = cpu.r.a >> 1;

    cpu.p.n = 0;
    cpu_lib.r.p.check_z(cpu, cpu.r.a);
  }
};

var LSR_absolute = {
  toString: function() { return "LSR" },
  bytes_required:3,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=6;

    var memory_location = (bytes[1]<<8)|bytes[0],
        shiftee;
    if(cpu.p.e||cpu.p.m) {
      shiftee = cpu.mmu.read_byte(memory_location);
      cpu.p.c = shiftee & 0x0001;
      shiftee = shiftee >> 1;
      cpu.mmu.store_byte(memory_location, shiftee);
    } else {
      cpu.cycle_count+=2;

      shiftee = cpu.mmu.read_word(memory_location);
      cpu.p.c = cpu.r.a & 0x01;
      shiftee = shiftee >> 1;
      cpu.mmu.store_word(memory_location, shiftee);
    }

    cpu.p.n = 0;
    cpu_lib.r.p.check_z(cpu, shiftee);
  }
};

var LSR_direct_page = {
  toString: function() { return "LSR" },
  bytes_required:2,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=5;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0] + cpu.r.d,
        shiftee;
    if(cpu.p.e||cpu.p.m) {
      shiftee = cpu.mmu.read_byte(memory_location);
      cpu.p.c = shiftee & 0x0001;
      shiftee = shiftee >> 1;
      cpu.mmu.store_byte(memory_location, shiftee);
    } else {
      cpu.cycle_count+=2;

      shiftee = cpu.mmu.read_word(memory_location);
      cpu.p.c = cpu.r.a & 0x01;
      shiftee = shiftee >> 1;
      cpu.mmu.store_word(memory_location, shiftee);
    }

    cpu.p.n = 0;
    cpu_lib.r.p.check_z(cpu, shiftee);
  }
};

var LSR_absolute_indexed_x =
  new cpu_lib.addressing.Absolute_indexed_x(LSR_absolute);

var LSR_direct_page_indexed_x =
  new cpu_lib.addressing.Direct_page_indexed(LSR_direct_page, 'x');

var EOR_const = {
  toString: function() { return "EOR" },
  bytes_required:function(cpu) {
    if(cpu.p.e||cpu.p.m) {
      return 2;
    } else {
      return 3;
    }
  },
  execute:function(cpu, bytes) {
    cpu.instruction_details += "<br />Exclusive OR with accumulator";
    if(!cpu.instruction_translated) {
      cpu.instruction_details += " (const)";
      cpu.instruction_translate = this.toString() + " #" + bytesToString(bytes);
      cpu.instruction_history += " " + this.toString() + " #" + bytesToString(bytes);
    }
    cpu.cycle_count+=2;

    if(cpu.p.e||cpu.p.m) {
      cpu.r.a ^= bytes[0];
      cpu.p.n = cpu.r.a >> 7;
    } else {
      cpu.cycle_count++;
      cpu.r.a ^= (bytes[1]<<8)|bytes[0];
      cpu.p.n = cpu.r.a >> 15;
    }

    cpu_lib.r.p.check_z(cpu, cpu.r.a);
  }
};

var EOR_absolute = new cpu_lib.addressing.Absolute(EOR_const);

var EOR_absolute_long = new cpu_lib.addressing.Absolute_long(EOR_const);

var EOR_absolute_long_indexed_x =
  new cpu_lib.addressing.Absolute_long_indexed_x(EOR_const);

var EOR_direct_page = new cpu_lib.addressing.Direct_page(EOR_const);

var EOR_direct_page_indirect =
  new cpu_lib.addressing.Direct_page_indirect(EOR_const);

var EOR_direct_page_indexed_x_indirect =
  new cpu_lib.addressing.Direct_page_indexed_x_indirect(EOR_const);

var EOR_direct_page_indirect_long_indexed_y =
  new cpu_lib.addressing.Direct_page_indirect_long_indexed_y(EOR_const);

var EOR_direct_page_indirect_long =
  new cpu_lib.addressing.Direct_page_indirect_long(EOR_const);

var EOR_direct_page_indirect_indexed_y =
  new cpu_lib.addressing.Direct_page_indirect_indexed_y(EOR_const);

var EOR_absolute_indexed_x =
  new cpu_lib.addressing.Absolute_indexed_x(EOR_absolute);

var EOR_absolute_indexed_y =
  new cpu_lib.addressing.Absolute_indexed_y(EOR_absolute);

var EOR_direct_page_indexed_x =
  new cpu_lib.addressing.Direct_page_indexed(EOR_direct_page, 'x');

var EOR_stack_relative = new cpu_lib.addressing.Stack_relative(EOR_const);

var EOR_stack_relative_indirect_indexed_y =
  new cpu_lib.addressing.Stack_relative_indirect_indexed_y(EOR_const);

var ORA_const= {
  toString: function() { return "ORA" },
  bytes_required:function(cpu) {
    if(cpu.p.e||cpu.p.m) {
      return 2;
    } else {
      return 3;
    }
  },
  execute:function(cpu, bytes) {
    cpu.instruction_details += "<br />OR with accumulator";
    if(!cpu.instruction_translated) {
      cpu.instruction_details += " (const)";
      cpu.instruction_translate = this.toString() + " #" + bytesToString(bytes);
      cpu.instruction_history += " " + this.toString() + " #" + bytesToString(bytes);
    }
    cpu.cycle_count+=2;

    if(cpu.p.e||cpu.p.m) {
      cpu.r.a |= bytes[0];
      cpu.p.n = cpu.r.a >> 7;
    } else {
      cpu.cycle_count++;

      cpu.r.a |= (bytes[1]<<8)|bytes[0];
      cpu.p.n = cpu.r.a >> 15;
    }

    cpu_lib.r.p.check_z(cpu, cpu.r.a);
  }
};

var ORA_absolute = new cpu_lib.addressing.Absolute(ORA_const);

var ORA_absolute_long = new cpu_lib.addressing.Absolute_long(ORA_const);

var ORA_absolute_long_indexed_x =
  new cpu_lib.addressing.Absolute_long_indexed_x(ORA_const);

var ORA_direct_page = new cpu_lib.addressing.Direct_page(ORA_const);

var ORA_direct_page_indirect =
  new cpu_lib.addressing.Direct_page_indirect(ORA_const);

var ORA_direct_page_indexed_x_indirect =
  new cpu_lib.addressing.Direct_page_indexed_x_indirect(ORA_const);

var ORA_direct_page_indirect_long =
  new cpu_lib.addressing.Direct_page_indirect_long(ORA_const);

var ORA_direct_page_indirect_long_indexed_y =
  new cpu_lib.addressing.Direct_page_indirect_long_indexed_y(ORA_const);

var ORA_direct_page_indirect_indexed_y =
  new cpu_lib.addressing.Direct_page_indirect_indexed_y(ORA_const);

var ORA_absolute_indexed_x =
  new cpu_lib.addressing.Absolute_indexed_x(ORA_absolute);

var ORA_absolute_indexed_y =
  new cpu_lib.addressing.Absolute_indexed_y(ORA_absolute);

var ORA_direct_page_indexed_x =
  new cpu_lib.addressing.Direct_page_indexed(ORA_direct_page,'x');

var ORA_stack_relative = new cpu_lib.addressing.Stack_relative(ORA_const);

var ORA_stack_relative_indirect_indexed_y =
  new cpu_lib.addressing.Stack_relative_indirect_indexed_y(ORA_const);

var AND_const= {
  toString: function() { return "AND" },
  bytes_required:function(cpu) {
    if(cpu.p.e||cpu.p.m) {
      return 2;
    } else {
      return 3;
    }
  },
  execute:function(cpu, bytes) {
    cpu.instruction_details += "<br />AND with accumulator";
    if(!cpu.instruction_translated) {
      cpu.instruction_details += " (const)";
      cpu.instruction_translate = this.toString() + " #" + bytesToString(bytes);
      cpu.instruction_history += " " + this.toString() + " #" + bytesToString(bytes);
    }
    cpu.cycle_count+=2;

    if(cpu.p.e||cpu.p.m) {
      cpu.r.a &= bytes[0];
      cpu.p.n = cpu.r.a >> 7;
    } else {
      cpu.cycle_count++;

      cpu.r.a &= (bytes[1]<<8)|bytes[0];
      cpu.p.n = cpu.r.a >> 15;
    }

    cpu_lib.r.p.check_z(cpu, cpu.r.a);
  }
};

var AND_absolute = new cpu_lib.addressing.Absolute(AND_const);

var AND_absolute_long = new cpu_lib.addressing.Absolute_long(AND_const);

var AND_absolute_long_indexed_x =
  new cpu_lib.addressing.Absolute_long_indexed_x(AND_const);

var AND_direct_page = new cpu_lib.addressing.Direct_page(AND_const);

var AND_direct_page_indirect =
  new cpu_lib.addressing.Direct_page_indirect(AND_const);

var AND_direct_page_indexed_x_indirect =
  new cpu_lib.addressing.Direct_page_indexed_x_indirect(AND_const);

var AND_direct_page_indirect_long =
  new cpu_lib.addressing.Direct_page_indirect_long(AND_const);

var AND_direct_page_indirect_long_indexed_y =
  new cpu_lib.addressing.Direct_page_indirect_long_indexed_y(AND_const);

var AND_direct_page_indirect_indexed_y =
  new cpu_lib.addressing.Direct_page_indirect_indexed_y(AND_const);

var AND_absolute_indexed_x =
  new cpu_lib.addressing.Absolute_indexed_x(AND_absolute);

var AND_absolute_indexed_y =
  new cpu_lib.addressing.Absolute_indexed_y(AND_absolute);

var AND_direct_page_indexed_x =
  new cpu_lib.addressing.Direct_page_indexed(AND_direct_page, 'x');

var AND_stack_relative = new cpu_lib.addressing.Stack_relative(AND_const);

var AND_stack_relative_indirect_indexed_y =
  new cpu_lib.addressing.Stack_relative_indirect_indexed_y(AND_const);

var CPX_const= {
  toString: function() { return "CPX" },
  bytes_required:function(cpu) {
    if(cpu.p.e||cpu.p.x) {
      return 2;
    } else {
      return 3;
    }
  },
  execute:function(cpu, bytes) {
    cpu.instruction_details += "<br />Compare value with X register";
    if(!cpu.instruction_translated) {
      cpu.instruction_details += " (const)";
      cpu.instruction_translate = this.toString() + " #" + bytesToString(bytes);
      cpu.instruction_history += " " + this.toString() + " #" + bytesToString(bytes);
    }
    cpu.cycle_count+=2;

    var result;
    if(cpu.p.e||cpu.p.x) {
      result = cpu.r.x - bytes[0];
      if(result<0) {
        cpu.p.c = 0;
        result = 0x100 + result;
      } else {
        cpu.p.c = 1;
      }
      cpu.p.n = result >> 7;
    } else {
      cpu.cycle_count++;

      result = cpu.r.x - ((bytes[1]<<8)|bytes[0]);
      if(result<0) {
        cpu.p.c = 0;
        result = 0x10000 + result;
      } else {
        cpu.p.c = 1;
      }
      cpu.p.n = result >> 15;
    }

    cpu_lib.r.p.check_z(cpu, result);
  }
};

var CPX_direct_page = new cpu_lib.addressing.Direct_page(CPX_const);

var CPX_absolute = new cpu_lib.addressing.Absolute(CPX_const);

var CPY_const= {
  toString: function() { return "CPY" },
  bytes_required:function(cpu) {
    if(cpu.p.e||cpu.p.x) {
      return 2;
    } else {
      return 3;
    }
  },
  execute:function(cpu, bytes) {
    cpu.instruction_details += "<br />Compare value with Y register";
    if(!cpu.instruction_translated) {
      cpu.instruction_details += " (const)";
      cpu.instruction_translate = this.toString() + " #" + bytesToString(bytes);
      cpu.instruction_history += " " + this.toString() + " #" + bytesToString(bytes);
    }
    cpu.cycle_count+=2;

    var result;
    if(cpu.p.e||cpu.p.x) {
      result = cpu.r.y - bytes[0];
      if(result<0) {
        cpu.p.c = 0;
        result = 0x100 + result;
      } else {
        cpu.p.c = 1;
      }
      cpu.p.n = result >> 7;
    } else {
      cpu.cycle_count++;
      result = cpu.r.y - ((bytes[1]<<8)|bytes[0]);
      if(result<0) {
        cpu.p.c = 0;
        result = 0x10000 + result;
      } else {
        cpu.p.c = 1;
      }
      cpu.p.n = result >> 15;
    }

    cpu_lib.r.p.check_z(cpu, result);
  }
};

var CPY_direct_page = new cpu_lib.addressing.Direct_page(CPY_const);

var CPY_absolute = new cpu_lib.addressing.Absolute(CPY_const);

var CMP_const= {
  toString: function() { return "CMP" },
  bytes_required:function(cpu) {
    if(cpu.p.e||cpu.p.m) {
      return 2;
    } else {
      return 3;
    }
  },
  execute:function(cpu, bytes) {
    cpu.instruction_details += "<br />Compare value with accumulator";
    if(!cpu.instruction_translated) {
      cpu.instruction_details += " (const)";
      cpu.instruction_translate = this.toString() + " #" + bytesToString(bytes);
      cpu.instruction_history += " " + this.toString() + " #" + bytesToString(bytes);
    }
    cpu.cycle_count+=2;

    var result;
    if(cpu.p.e||cpu.p.m) {
      result = cpu.r.a - bytes[0];
      if(result<0) {
        cpu.p.c = 0;
        result = 0x100 + result;
      } else {
        cpu.p.c = 1;
      }
      cpu.p.n = result >> 7;
    } else {
      cpu.cycle_count++;

      result = cpu.r.a - ((bytes[1]<<8)|bytes[0]);
      if(result<0) {
        cpu.p.c = 0;
        result = 0x10000 + result;
      } else {
        cpu.p.c = 1;
      }
      cpu.p.n = result >> 15;
    }

    cpu_lib.r.p.check_z(cpu, result);
  }
};

var CMP_direct_page = new cpu_lib.addressing.Direct_page(CMP_const);

var CMP_direct_page_indexed_x =
  new cpu_lib.addressing.Direct_page_indexed(CMP_direct_page, 'x');

var CMP_direct_page_indirect =
  new cpu_lib.addressing.Direct_page_indirect(CMP_const);

var CMP_direct_page_indexed_x_indirect =
  new cpu_lib.addressing.Direct_page_indexed_x_indirect(CMP_const);

var CMP_direct_page_indirect_long =
  new cpu_lib.addressing.Direct_page_indirect_long(CMP_const);

var CMP_direct_page_indirect_long_indexed_y =
  new cpu_lib.addressing.Direct_page_indirect_long_indexed_y(CMP_const);

var CMP_direct_page_indirect_indexed_y =
  new cpu_lib.addressing.Direct_page_indirect_indexed_y(CMP_const);

var CMP_absolute = new cpu_lib.addressing.Absolute(CMP_const);

var CMP_absolute_long = new cpu_lib.addressing.Absolute_long(CMP_const);

var CMP_absolute_long_indexed_x =
  new cpu_lib.addressing.Absolute_long_indexed_x(CMP_const);

var CMP_absolute_indexed_x =
  new cpu_lib.addressing.Absolute_indexed_x(CMP_absolute);

var CMP_absolute_indexed_y =
  new cpu_lib.addressing.Absolute_indexed_y(CMP_absolute);

var CMP_stack_relative = new cpu_lib.addressing.Stack_relative(CMP_const);

var CMP_stack_relative_indirect_indexed_y =
  new cpu_lib.addressing.Stack_relative_indirect_indexed_y(CMP_const);

var SBC_const= {
  toString: function() { return "SBC" },
  bytes_required:function(cpu) {
    if(cpu.p.e||cpu.p.m) {
      return 2;
    } else {
      return 3;
    }
  },
  execute:function(cpu, bytes) {
    cpu.instruction_details += "<br />Substract value from accumulator";
    if(!cpu.instruction_translated) {
      cpu.instruction_details += " (const)";
      cpu.instruction_translate = this.toString() + " #" + bytesToString(bytes);
      cpu.instruction_history += " " + this.toString() + " #" + bytesToString(bytes);
    }
    cpu.cycle_count+=2;

    var old_a = cpu.r.a,
        temp = 0;
    if(cpu.p.c===0)
      temp = 1;

    if(cpu.p.e||cpu.p.m) {
      if(cpu.p.d) {
         // Form a decimal number out of a.
        var ones = cpu.r.a & 0x0f,
            tens = cpu.r.a >> 4,
            dec_a = (tens*10)+ones;

        // Form a decimal number out of the argument.
        ones = bytes[0] & 0x0f;
        tens = bytes[0] >> 4;
        var result = dec_a - ((tens*10)+ones) - temp;

        // Check for decimal overflow.
        if(result<0) {
          result += 100;
          cpu.p.c = 0;
        } else {
          cpu.p.c = 1;
        }
        var digits = result.toString(10).split(""),
            i;
        cpu.r.a = 0;
        for(i=0;i<digits.length;i++) {
          cpu.r.a += (digits[i]-0)*Math.pow(16,digits.length-i-1);
        }
      } else {
        cpu.r.a -= bytes[0] - temp;
        if(cpu.r.a < 0) {
          cpu.p.c = 0;
          cpu.r.a = 0x100 + cpu.r.a;
        } else {
          cpu.p.c = 1;
        }
        cpu.p.n = cpu.r.a >> 7;

        // Check for signed overflow.
        // If they started with the same sign and then the resulting sign is
        // different then we have a signed overflow.
        if((!((old_a ^ bytes[0]) & 0x80)) && ((cpu.r.a ^ old_a) & 0x80)) {
          cpu.p.v = 1;
        } else {
          cpu.p.v = 0;
        }
      }
    } else {
      cpu.cycle_count++;

      var argument = (bytes[1]<<8)|bytes[0];
      temp = 0;

      if(cpu.p.c===0)
        temp = 1;

      if(cpu.p.d) {
        // Form a decimal number out of a.
        var ones = cpu.r.a & 0xf,
            tens = (cpu.r.a >>4) & 0xf,
            hundreds = (cpu.r.a >> 8) & 0xf,
            thousands = (cpu.r.a >> 12) & 0xf,
            dec_a = (thousands*1000)+(hundreds*100)+(tens*10)+ones;

        // Form a decimal number out of the argument.
        ones = argument & 0xf;
        tens = (argument >> 4) & 0xf;
        hundreds = (argument >> 8) & 0xf;
        thousands = (argument >> 12) & 0xf;
        var dec_arg = (thousands*1000)+(hundreds*100)+(tens*10)+ones,
            result = dec_a - dec_arg - temp;
        // Check for decimal overflow.
        if(result<0) {
          result += 10000;
          cpu.p.c = 0;
        } else {
          cpu.p.c = 1;
        }
        var digits = result.toString(10).split(""),
            i;
        cpu.r.a = 0;
        for(i=0;i<digits.length;i++) {
          cpu.r.a += (digits[i]-0)*Math.pow(16,digits.length-i-1);
        }
      } else {
        cpu.r.a -= argument - temp;
        if(cpu.r.a < 0) {
          cpu.p.c = 0;
          cpu.r.a = 0x10000 + cpu.r.a;
        } else {
          cpu.p.c = 1;
        }
        cpu.p.n = cpu.r.a >> 15;

        // Check for signed overflow.
        // If they started with the same sign and then the resulting sign is
        // different then we have a signed overflow.
        if((!((old_a ^ argument) & 0x8000)) && ((cpu.r.a ^ old_a) & 0x8000)) {
          cpu.p.v = 1;
        } else {
          cpu.p.v = 0;
        }
      }
    }

    cpu_lib.r.p.check_z(cpu, cpu.r.a);
  }
};

var SBC_direct_page = new cpu_lib.addressing.Direct_page(SBC_const);

var SBC_absolute = new cpu_lib.addressing.Absolute(SBC_const);

var SBC_absolute_long = new cpu_lib.addressing.Absolute_long(SBC_const);

var SBC_absolute_long_indexed_x =
  new cpu_lib.addressing.Absolute_long_indexed_x(SBC_const);

var SBC_direct_page_indexed_x_indirect =
  new cpu_lib.addressing.Direct_page_indexed_x_indirect(SBC_const);

var SBC_direct_page_indirect_long =
  new cpu_lib.addressing.Direct_page_indirect_long(SBC_const);

var SBC_direct_page_indirect_long_indexed_y =
  new cpu_lib.addressing.Direct_page_indirect_long_indexed_y(SBC_const);

var SBC_direct_page_indirect_indexed_y =
  new cpu_lib.addressing.Direct_page_indirect_indexed_y(SBC_const);

var SBC_absolute_indexed_x =
  new cpu_lib.addressing.Absolute_indexed_x(SBC_absolute);

var SBC_absolute_indexed_y =
  new cpu_lib.addressing.Absolute_indexed_y(SBC_absolute);

var SBC_direct_page_indexed_x =
  new cpu_lib.addressing.Direct_page_indexed(SBC_direct_page, 'x');

var SBC_direct_page_indirect =
  new cpu_lib.addressing.Direct_page_indirect(SBC_const);

var SBC_stack_relative = new cpu_lib.addressing.Stack_relative(SBC_const);

var SBC_stack_relative_indirect_indexed_y =
  new cpu_lib.addressing.Stack_relative_indirect_indexed_y(SBC_const);

var ADC_const = {
  toString: function() { return "ADC" },
  bytes_required:function(cpu) {
    if(cpu.p.e||cpu.p.m) {
      return 2;
    } else {
      return 3;
    }
  },
  execute:function(cpu, bytes) {
    cpu.instruction_details += "<br />Add with cary value to accumulator";
    if(!cpu.instruction_translated) {
      cpu.instruction_details += " (const)";
      cpu.instruction_translate = this.toString() + " #" + bytesToString(bytes);
      cpu.instruction_history += " " + this.toString() + " #" + bytesToString(bytes);
    }
    var old_a = cpu.r.a;
    if(cpu.p.e||cpu.p.m) {
      cpu.cycle_count+=2;

      if(cpu.p.d) {
        // Form a decimal number out of a.
        var ones = cpu.r.a & 0x0f,
            tens = cpu.r.a >>4,
            dec_a = (tens*10)+ones;

        // Form a decimal number out of the argument.
        ones = bytes[0] & 0x0f;
        tens = bytes[0] >>4;
        var result = dec_a + ((tens*10)+ones) + cpu.p.c;
        // Check for decimal overflow.
        if(result>99) {
          result -= 99;
          cpu.p.c = 1;
        } else {
          cpu.p.c = 0;
        }
        var digits = result.toString(10).split(""),
            i;
        cpu.r.a = 0;
        for(i=0;i<digits.length;i++) {
          cpu.r.a += (digits[i]-0)*Math.pow(16,digits.length-i-1);
        }
      } else {
        cpu.r.a += bytes[0] + cpu.p.c;
        if(cpu.r.a & 0x100) {
          cpu.p.c = 1;
        } else {
          cpu.p.c = 0;
        }
        cpu.r.a &= 0xff;
        cpu.p.n = cpu.r.a >> 7;

        // Check for signed overflow.
        // If they started with the same sign and then the resulting sign is
        // different then we have a signed overflow.
        if((!((old_a ^ bytes[0]) & 0x80)) && ((cpu.r.a ^ old_a) & 0x80)) {
          cpu.p.v = 1;
        } else {
          cpu.p.v = 0;
        }
      }
    } else {
      cpu.cycle_count+=3;
      var argument = (bytes[1]<<8)|bytes[0];
      if(cpu.p.d) {
        // Form a decimal number out of a.
        var ones = cpu.r.a & 0xf,
            tens = (cpu.r.a >>4) & 0xf,
            hundreds = (cpu.r.a >> 8) & 0xf,
            thousands = (cpu.r.a >> 12) & 0xf,
            dec_a = (thousands*1000)+(hundreds*100)+(tens*10)+ones;

        // Form a decimal number out of the argument.
        ones = argument & 0xf;
        tens = (argument >> 4) & 0xf;
        hundreds = (argument >> 8) & 0xf;
        thousands = (argument >> 12) & 0xf;
        var dec_arg = (thousands*1000)+(hundreds*100)+(tens*10)+ones,
            result = dec_a + dec_arg + cpu.p.c;
        // Check for decimal overflow.
        if(result>9999) {
          result -= 9999;
          cpu.p.c = 1;
        } else {
          cpu.p.c = 0;
        }
        var digits = result.toString(10).split(""),
            i;
        cpu.r.a = 0;
        for(i=0;i<digits.length;i++) {
          cpu.r.a += (digits[i]-0)*Math.pow(16,digits.length-i-1);
        }
      } else {
        cpu.r.a += argument + cpu.p.c;
        if(cpu.r.a & 0x10000) {
          cpu.p.c = 1;
        } else {
          cpu.p.c = 0;
        }
        cpu.r.a &= 0xffff;
        cpu.p.n = cpu.r.a >> 15;

        // Check for signed overflow.
        // If they started with the same sign and then the resulting sign is
        // different then we have a signed overflow.
        if((!((old_a ^ argument) & 0x8000)) && ((cpu.r.a ^ old_a) & 0x8000)) {
          cpu.p.v = 1;
        } else {
          cpu.p.v = 0;
        }
      }
    }

    cpu_lib.r.p.check_z(cpu, cpu.r.a);
  }
};

var ADC_absolute = new cpu_lib.addressing.Absolute(ADC_const);

var ADC_absolute_long = new cpu_lib.addressing.Absolute_long(ADC_const);

var ADC_absolute_long_indexed_x =
  new cpu_lib.addressing.Absolute_long_indexed_x(ADC_const);

var ADC_direct_page = new cpu_lib.addressing.Direct_page(ADC_const);

var ADC_direct_page_indirect =
  new cpu_lib.addressing.Direct_page_indirect(ADC_const);

var ADC_direct_page_indexed_x_indirect =
  new cpu_lib.addressing.Direct_page_indexed_x_indirect(ADC_const);

var ADC_direct_page_indirect_long_indexed_y =
  new cpu_lib.addressing.Direct_page_indirect_long_indexed_y(ADC_const);

var ADC_direct_page_indirect_long =
  new cpu_lib.addressing.Direct_page_indirect_long(ADC_const);

var ADC_direct_page_indirect_indexed_y =
  new cpu_lib.addressing.Direct_page_indirect_indexed_y(ADC_const);

var ADC_absolute_indexed_x =
  new cpu_lib.addressing.Absolute_indexed_x(ADC_absolute);

var ADC_absolute_indexed_y =
  new cpu_lib.addressing.Absolute_indexed_y(ADC_absolute);

var ADC_direct_page_indexed_x =
  new cpu_lib.addressing.Direct_page_indexed(ADC_direct_page, 'x');

var ADC_stack_relative = new cpu_lib.addressing.Stack_relative(ADC_const);

var ADC_stack_relative_indirect_indexed_y =
  new cpu_lib.addressing.Stack_relative_indirect_indexed_y(ADC_const);

var BMI = new cpu_lib.branch.Branch('n', true);

var BPL = new cpu_lib.branch.Branch('n', false);

var BVC = new cpu_lib.branch.Branch('v', false);

var BVS = new cpu_lib.branch.Branch('v', true);

var BCC = new cpu_lib.branch.Branch('c', false);

var BCS = new cpu_lib.branch.Branch('c', true);

var BEQ = new cpu_lib.branch.Branch('z', true);

var BNE = new cpu_lib.branch.Branch('z', false);

var BRA = new cpu_lib.branch.Branch();

var BRL = {
  toString: function() { return "BRL" },
  bytes_required:3,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=4;

    // Handle double byte two's complement numbers as the branch argument.
    var num = (bytes[1]<<8)|bytes[0];
    if(num<=32767) {
      cpu.r.pc+=num;
      cpu.r.pc&=0xffff;
    } else {
      cpu.r.pc-=65536-num;
      if(cpu.r.pc<0)
        cpu.r.pc+=0xffff;
    }
  }
};


var JMP_absolute_indirect= {
  toString: function() { return "JMP" },
  bytes_required:3,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=5;

    cpu.r.pc = cpu.mmu.read_word((bytes[1]<<8)|bytes[0]);
  }
};

var JMP_absolute_long= {
  toString: function() { return "JMP" },
  bytes_required:4,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=4;

    cpu.r.k = bytes[2];
    cpu.r.pc = (bytes[1]<<8)|bytes[0];
  }
};

var JMP_absolute_indirect_long= {
  toString: function() { return "JMP" },
  bytes_required:3,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=6;

    var memory_location = (bytes[1]<<8)|bytes[0];
    cpu.r.pc = cpu.mmu.read_word(memory_location);
    cpu.r.k = cpu.mmu.read_byte(memory_location+2);
  }
};

var JMP_absolute_indexed_x_indirect= {
  toString: function() { return "JMP" },
  bytes_required:3,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=6;

    var memory_location = ((bytes[1]<<8)|bytes[0])+cpu.r.x,
        bank = cpu.r.k;
    if(memory_location&0x10000) {
       bank++;
    }
    memory_location &= 0xffff;
    var indirect_location = cpu.mmu.read_word_long(memory_location, bank),
        low_byte = cpu.mmu.read_byte(indirect_location);
    bank = cpu.r.k;
    if(indirect_location===0xffff) {
      indirect_location = 0;
      bank++;
    } else {
      indirect_location++;
    }
    var high_byte = cpu.mmu.read_byte_long(indirect_location, bank);
    cpu.r.pc =  (high_byte<<8)|low_byte;
  }
};

var JMP_absolute= {
  toString: function() { return "JMP" },
  bytes_required:3,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=3;

    cpu.r.pc = (bytes[1]<<8)|bytes[0];
  }
};

var TYA = {
  toString: function() { return "TYA" },
  bytes_required:function() {
    return 1;
  },
  execute:function(cpu) {
    cpu.cycle_count+=2;
    if(cpu.p.e||cpu.p.m) {
      if(cpu.p.e||cpu.p.x) {
        // 8-bit index register to 8-bit accumulator.
        cpu.r.a = cpu.r.y;
      } else {
        // 16-bit index register to 8-bit accumulator.
        cpu.r.a = cpu.r.y & 0x00ff;
      }
      cpu.p.n = cpu.r.a >> 7;
    } else {
      // 8-bit index register to 16-bit accumulator.
      // 16-bit index register to 16-bit accumulator.
      cpu.r.a = cpu.r.y;
      cpu.p.n = cpu.r.a >> 15;
    }
    cpu_lib.r.p.check_z(cpu, cpu.r.a);
    cpu.instruction_history += " TYA";
    cpu.instruction_details += "<br />Transfer Y Register to Accumulator";
  }
};

var TAY = {
  toString: function() { return "TAY" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=2;
    if(cpu.p.e||cpu.p.m) {
      if(cpu.p.e||cpu.p.x) {
        // 8-bit accumulator to 8-bit x index register.
        cpu.r.y = cpu.r.a;
        cpu.p.n = cpu.r.y >> 7;
      } else {
        // 8-bit accumulator to 16-bit x index register.
        cpu.r.y = cpu.r.b;  // Transfer b as high-byte of x.
        cpu.r.y |= cpu.r.a; // Use the bitwise or to add a as the low-byte.
        cpu.p.n = cpu.r.y >> 15;
      }
    } else {
      if(cpu.p.x) {
        // 16-bit accumulator to 8-bit x index register.
        cpu.r.y = cpu.r.a & 0x00ff; // Transfer only the low-byte to x.
        cpu.p.n = cpu.r.y >> 7;
      } else {
        // 16-bit accumulator to 16-bit x index register.
        cpu.r.y = cpu.r.a;
        cpu.p.n = cpu.r.y >> 15;
      }
    }

    cpu_lib.r.p.check_z(cpu, cpu.r.y);
    cpu.instruction_history += " TAY";
    cpu.instruction_details += "<br />Transfer Accumulator to Y Register";
  }
};


var TXA = {
  toString: function() { return "TXA" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=2;
    if(cpu.p.e||cpu.p.m) {
      if(cpu.p.e||cpu.p.x) {
        // 8-bit index register to 8-bit accumulator.
        cpu.r.a = cpu.r.x;
      } else {
        // 16-bit index register to 8-bit accumulator.
        cpu.r.a = cpu.r.x & 0x00ff;
      }
      cpu.p.n = cpu.r.a >> 7;
    } else {
      // 8-bit index register to 16-bit accumulator.
      // 16-bit index register to 16-bit accumulator.
      cpu.r.a = cpu.r.x;
      cpu.p.n = cpu.r.a >> 15;
    }
    cpu_lib.r.p.check_z(cpu, cpu.r.a);
    cpu.instruction_history += " TXA";
    cpu.instruction_details += "<br />Transfer X Register to Accumulator";
  }
};

var TAX = {
  toString: function() { return "TAX" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=2;
    if(cpu.p.e||cpu.p.m) {
      if(cpu.p.e||cpu.p.x) {
        // 8-bit accumulator to 8-bit x index register.
        cpu.r.x = cpu.r.a;
        cpu.p.n = cpu.r.x >> 7;
      } else {
        // 8-bit accumulator to 16-bit x index register.
        cpu.r.x = cpu.r.b;  // Transfer b as high-byte of x.
        cpu.r.x |= cpu.r.a; // Use the bitwise or to add a as the low-byte.
        cpu.p.n = cpu.r.x >> 15;
      }
    } else {
      if(cpu.p.x) {
        // 16-bit accumulator to 8-bit x index register.
        cpu.r.x = cpu.r.a & 0x00ff; // Transfer only the low-byte to x.
        cpu.p.n = cpu.r.x >> 7;
      } else {
        // 16-bit accumulator to 16-bit x index register.
        cpu.r.x = cpu.r.a;
        cpu.p.n = cpu.r.x >> 15;
      }
    }
    cpu_lib.r.p.check_z(cpu, cpu.r.x);
    cpu.instruction_history += " TAX";
    cpu.instruction_details += "<br />Transfer Accumulator to X Register";
  }
};

var TXY = {
  toString: function() { return "TXY" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=2;
    cpu.r.y = cpu.r.x;
    cpu_lib.r.p.check_z(cpu, cpu.r.y);

    if(cpu.p.e||cpu.p.x) {
      cpu.p.n = cpu.r.y >> 7;
    } else {
      cpu.p.n = cpu.r.y >> 15;
    }
    cpu.instruction_history += " TXY";
    cpu.instruction_details += "<br />Transfer X Register to Y Register";
  }
};

var TYX = {
  toString: function() { return "TYX" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=2;
    cpu.r.x = cpu.r.y;
    cpu_lib.r.p.check_z(cpu, cpu.r.x);

    if(cpu.p.e||cpu.p.x) {
      cpu.p.n = cpu.r.y >> 7;
    } else {
      cpu.p.n = cpu.r.y >> 15;
    }
    cpu.instruction_history += " TYX";
    cpu.instruction_details += "<br />Transfer Y Register to X Register";
  }
};

var TCD = {
  toString: function() { return "TCD" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=2;

    // Transfers 16-bits regardless of setting.
    if(cpu.p.e||cpu.p.m) {
      cpu.r.d = (cpu.r.b<<8)|cpu.r.a;
    } else {
      cpu.r.d = cpu.r.a;
    }

    cpu.p.n = cpu.r.d >> 15;

    cpu_lib.r.p.check_z(cpu, cpu.r.d);
    cpu.instruction_history += " TCD";
    cpu.instruction_details += "<br />Transfer C acc. to Direct Register";
  }
};

var TDC = {
  toString: function() { return "TDC" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=2;

    // Transfers 16-bits regardless of setting.
    if(cpu.p.e||cpu.p.m) {
      cpu.r.a = cpu.r.d & 0xff;
      cpu.r.b = cpu.r.d >> 8;
      cpu.p.n = cpu.r.b >> 7;
    } else {
      cpu.r.a = cpu.r.d;
      cpu.p.n = cpu.r.a >> 7;
    }

    cpu_lib.r.p.check_z(cpu, cpu.r.a);
    cpu.instruction_history += " TDC";
    cpu.instruction_details += "<br />Transfer Direct Register to C acc.";
  }
};

var TCS = {
  toString: function() { return "TCS" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=2;
    if(cpu.p.e||!cpu.p.m) {
      cpu.r.s = cpu.r.a;
    } else {
      cpu.r.s = (cpu.r.b<<8)|cpu.r.a;
    }
    cpu.instruction_history += " TCS";
    cpu.instruction_details += "<br />Transfer C acc. to Stack Pointer";
  }
};

var TSC = {
  toString: function() { return "TSC" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=2;

    if(cpu.p.e) {
      cpu.r.b = 1;
      cpu.r.a = cpu.r.s;
      // TODO: Figure out if in emulation mode the z and n bits should always
      // be set to zero here as a 1 is transferred to b.
      cpu.p.n = 0;
      cpu.p.z = 0;
    } else {
      if(cpu.p.m) {
        cpu.r.a = cpu.r.s & 0xff;
        cpu.r.b = cpu.r.s >> 8;
        cpu.p.n = cpu.r.b >> 7;
      } else {
        cpu.r.a = cpu.r.s;
        cpu.p.n = cpu.r.a >> 15;
      }

      cpu_lib.r.p.check_z(cpu, cpu.r.s);
    }
    cpu.instruction_history += " TSC";
    cpu.instruction_details += "<br />Transfer Stack Pointer to C acc.";
  }
};

var STZ_absolute= {
  toString: function() { return "STZ" },
  bytes_required:3,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=4;

    var memory_location = (bytes[1]<<8)|bytes[0];
    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.store_byte(memory_location, 0);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(memory_location, 0);
    }
  }
};

var STZ_direct_page= {
  toString: function() { return "STZ" },
  bytes_required:2,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=3;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0]+cpu.r.d;
    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.store_byte(memory_location, 0);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(memory_location, 0);
    }
  }
};

var STZ_absolute_indexed_x= {
  toString: function() { return "STZ" },
  bytes_required:3,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=5;

    var memory_location = ((bytes[1]<<8)|bytes[0])+cpu.r.x;
    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.store_byte(memory_location, 0);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(memory_location, 0);
    }
  }
};

var STZ_direct_page_indexed_x= {
  toString: function() { return "STZ" },
  bytes_required:2,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=4;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0]+cpu.r.d+cpu.r.x;
    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.store_byte(memory_location, 0);
    } else {
      cpu.cycle_count++;

      // Check for overflow.
      var overflow_check = memory_location - 0xffff;
      if(overflow_check > 0) {
        memory_location = overflow_check-1;
      }
      cpu.mmu.store_byte(memory_location, 0);
      // Check for potential overflow again.
      if(memory_location===0xffff) {
       memory_location = 0;
      } else {
        memory_location++;
      }
      cpu.mmu.store_byte(memory_location, 0);
    }
  }
};

var STA_direct_page_indirect= {
  toString: function() { return "STA" },
  bytes_required:2,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=5;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0] + cpu.r.d,
        absolute_location = cpu.mmu.read_word(memory_location);
    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.store_byte(absolute_location, cpu.r.a);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(absolute_location, cpu.r.a);
    }
  }
};

var STA_direct_page_indirect_long= {
  toString: function() { return "STA" },
  bytes_required:2,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=6;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0] + cpu.r.d,
        absolute_location = cpu.mmu.read_word(memory_location),
        bank_byte = cpu.mmu.read_byte(memory_location+2);
    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.store_byte_long(absolute_location, bank_byte,
                              cpu.r.a);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word_long(absolute_location, bank_byte, cpu.r.a);
    }
  }
};

var STA_direct_page_indirect_long_indexed_y= {
  toString: function() { return "STA" },
  bytes_required:2,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=6;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0] + cpu.r.d,
        bank_byte = cpu.mmu.read_byte(memory_location+2),
        absolute_location = cpu.mmu.read_word(memory_location) + cpu.r.y;
    if(absolute_location >> 16) {
      bank_byte++;
      absolute_location &= 0xffff;
    }
    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.store_byte_long(absolute_location, bank_byte, cpu.r.a);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_byte_long(absolute_location, bank_byte, cpu.r.a&0xff);
      absolute_location++;
      if(absolute_location >> 16) {
        bank_byte++;
      }
      cpu.mmu.store_byte_long(absolute_location, bank_byte, cpu.r.a>>8);
    }
  }
};

var STA_direct_page_indirect_indexed_y= {
  toString: function() { return "STA" },
  bytes_required:2,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=6;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0] + cpu.r.d,
        absolute_location = cpu.mmu.read_word(memory_location) + cpu.r.y;
    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.store_byte(absolute_location, cpu.r.a);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(absolute_location, cpu.r.a);
    }
  }
};

var STA_stack_relative= {
  toString: function() { return "STA" },
  bytes_required:2,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=4;

    if(cpu.p.e) {
      cpu.mmu.store_byte(0x100 | ((cpu.r.s + bytes[0]) & 0xff), cpu.r.a);
    } else {
      if(cpu.p.m) {
        cpu.mmu.store_byte(cpu.r.s + bytes[0], cpu.r.a);
      } else {
        cpu.cycle_count++;

        cpu.mmu.store_word(cpu.r.s + bytes[0], cpu.r.a);
      }
    }
  }
};

var STA_stack_relative_indirect_indexed_y= {
  toString: function() { return "STA" },
  bytes_required:2,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=7;

    // TODO: fixes below are not well-tested!
    if(cpu.p.e) {
      var location_loc = 0x100 | ((cpu.r.s + bytes[0]) & 0xff),
          low_byte =  cpu.mmu.read_byte(location_loc),
          high_byte;
      if(location_loc===0x1ff) {
        high_byte = cpu.mmu.read_byte(0x100);
      } else {
        high_byte = cpu.mmu.read_byte(location_loc+1);
      }
      var absolute_location = ((high_byte<<8)|low_byte)+cpu.r.y;
      cpu.mmu.store_byte(absolute_location, cpu.r.a);
    } else {
      var location_loc = cpu.r.s + bytes[0],
          absolute_location = cpu.mmu.read_word(location_loc)+cpu.r.y;
      if(cpu.p.m) {
        cpu.mmu.store_byte(absolute_location, cpu.r.a);
      } else {
        cpu.cycle_count++;
        cpu.mmu.store_word(absolute_location, cpu.r.a);
      }
    }
  }
};

var NOP = {
  toString: function() { return "NOP" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=2;
    cpu.instruction_history += " NOP";
    cpu.instruction_details += "<br />No OPeration"
  }
};

var LDY_const= {
  toString: function() { return "LDY" },
  bytes_required:function(cpu) {
    if(cpu.p.e||cpu.p.x)
      return 2;
    else
      return 3;
  },
  execute:function(cpu, bytes) {
    cpu.instruction_details += "<br />Load value to register Y";
    if(!cpu.instruction_translated) {
      cpu.instruction_details += " (const)";
      cpu.instruction_translate = this.toString() + " #" + bytesToString(bytes);
      cpu.instruction_history += " " + this.toString() + " #" + bytesToString(bytes);
    }
    cpu.cycle_count+=2;

    if(cpu.p.e||cpu.p.x) {
      cpu.r.y = bytes[0];
      cpu.p.n = cpu.r.y >> 7;
    } else {
      cpu.cycle_count++;

      cpu.r.y = (bytes[1]<<8)|bytes[0];
      cpu.p.n = cpu.r.y >> 15;
    }
    cpu_lib.r.p.check_z(cpu, cpu.r.y);
  }
};

var LDY_absolute = new cpu_lib.addressing.Absolute(LDY_const);

var LDY_absolute_indexed_x =
  new cpu_lib.addressing.Absolute_indexed_x(LDY_absolute);

var LDY_direct_page = new cpu_lib.addressing.Direct_page(LDY_const);

var LDY_direct_page_indexed_x =
  new cpu_lib.addressing.Direct_page_indexed(LDY_direct_page, 'x');

// Implement the decrement instructions as increment instructions that
// increment by negative one.
var DEX = new cpu_lib.inc.INC_register('x', 'x', -1);

var DEY = new cpu_lib.inc.INC_register('y', 'x', -1);

var DEC_accumulator = new cpu_lib.inc.INC_register('a', 'm', -1);

var DEC_memory = new cpu_lib.inc.INC_memory(-1);

var DEC_absolute = new cpu_lib.addressing.Absolute(DEC_memory);

var DEC_absolute_indexed_x =
  new cpu_lib.addressing.Absolute_indexed_x(DEC_absolute, true);

var DEC_direct_page = new cpu_lib.addressing.Direct_page(DEC_memory);

var DEC_direct_page_indexed_x =
  new cpu_lib.addressing.Direct_page_indexed(DEC_direct_page, 'x');

var INX = new cpu_lib.inc.INC_register('x', 'x');

var INY = new cpu_lib.inc.INC_register('y', 'x');

var INC_accumulator = new cpu_lib.inc.INC_register('a', 'm');

var INC_memory = new cpu_lib.inc.INC_memory();

var INC_absolute = new cpu_lib.addressing.Absolute(INC_memory);

var INC_absolute_indexed_x =
  new cpu_lib.addressing.Absolute_indexed_x(INC_absolute, true);

var INC_direct_page = new cpu_lib.addressing.Direct_page(INC_memory);

var INC_direct_page_indexed_x =
  new cpu_lib.addressing.Direct_page_indexed(INC_direct_page, 'x');

var STA_direct_page_indexed_x= {
  toString: function() { return "STA" },
  bytes_required:2,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=4;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0]+cpu.p.d+cpu.r.x;
    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.store_byte(memory_location, cpu.r.a);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(memory_location, cpu.r.a);
    }
  }
};

var STA_direct_page_indexed_x_indirect= {
  toString: function() { return "STA" },
  bytes_required:2,

  execute:function(cpu, bytes) {
    cpu.cycle_count+=6;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    if(cpu.p.e) {
      var memory_location = (bytes[0] + cpu.r.x) & 0xff,
          low_byte_loc = cpu.mmu.read_byte_long(memory_location+cpu.r.d, 0),
          high_byte_read_loc = ((memory_location+1)&0xff)+cpu.r.d,
          high_byte_loc = cpu.mmu.read_byte_long(high_byte_read_loc, 0);
      cpu.mmu.store_byte((high_byte_loc<<8) | low_byte_loc, cpu.r.a);
    } else if(cpu.p.m) {
      var memory_location = bytes[0] + cpu.r.d + cpu.r.x;
      cpu.mmu.store_byte(cpu.mmu.read_word(memory_location), cpu.r.a);
    } else {
      cpu.cycle_count++;

      var memory_location = bytes[0] + cpu.r.d + cpu.r.x,
          absolute_location = cpu.mmu.read_word(memory_location),
          low_byte = cpu.mmu.read_byte(absolute_location),
          high_byte;
      absolute_location++;
      if(absolute_location&0x10000) {
        high_byte = cpu.mmu.read_byte_long(absolute_location, cpu.r.dbr+1);
      } else {
        high_byte = cpu.mmu.read_byte(absolute_location);
      }
      var storage_location = (high_byte<<8) | low_byte;
      cpu.mmu.store_byte(storage_location, cpu.r.a & 0xff);
      storage_location++;
      if(storage_location&0x10000) {
        cpu.mmu.store_byte_long(storage_location, cpu.r.dbr+1, cpu.r.a >> 8);
      } else {
        cpu.mmu.store_byte(storage_location, cpu.r.a >> 8);
      }
    }
  }
};

var STA_direct_page= {
  toString: function() { return "STA" },
  bytes_required:2,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=3;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0]+cpu.p.d;
    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.store_byte(memory_location, cpu.r.a);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(memory_location, cpu.r.a);
    }
  }
};

var STA_absolute_indexed_x= {
  toString: function() { return "STA" },
  bytes_required:3,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=5;

    var memory_location = ((bytes[1]<<8)|bytes[0])+cpu.r.x;
    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.store_byte(memory_location, cpu.r.a);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(memory_location, cpu.r.a);
    }
  }
};

var STA_absolute_indexed_y= {
  toString: function() { return "STA" },
  bytes_required:3,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=5;

    var memory_location = ((bytes[1]<<8)|bytes[0])+cpu.r.y;
    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.store_byte(memory_location, cpu.r.a);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(memory_location, cpu.r.a);
    }
  }
};

var STY_direct_page_indexed_x= {
  toString: function() { return "STA" },
  bytes_required:2,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=4;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0]+cpu.p.d+cpu.r.x;
    if(cpu.p.e||cpu.p.x) {
      cpu.mmu.store_byte(memory_location, cpu.r.y);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(memory_location, cpu.r.y);
    }
  }
};

var STY_direct_page= {
  toString: function() { return "STY" },
  bytes_required:2,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=3;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0]+cpu.p.d;
    if(cpu.p.e||cpu.p.x) {
      cpu.mmu.store_byte(memory_location, cpu.r.y);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(memory_location, cpu.r.y);
    }
  }
};

var STY_absolute= {
  toString: function() { return "STY" },
  bytes_required:3,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=4;

    var memory_location = (bytes[1]<<8)|bytes[0];
    if(cpu.p.e||cpu.p.x) {
      cpu.mmu.store_byte(memory_location, cpu.r.y);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(memory_location, cpu.r.y);
    }
  }
};

var STX_direct_page_indexed_y= {
  toString: function() { return "STX" },
  bytes_required:2,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=4;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0]+cpu.p.d+cpu.r.y;
    if(cpu.p.e||cpu.p.x) {
      cpu.mmu.store_byte(memory_location, cpu.r.x);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(memory_location, cpu.r.x);
    }
  }
};

var STX_direct_page= {
  toString: function() { return "STX" },
  bytes_required:2,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=3;

    if((cpu.r.d&0xff)!==0)
      cpu.cycle_count++;

    var memory_location = bytes[0]+cpu.p.d;
    if(cpu.p.e||cpu.p.x) {
      cpu.mmu.store_byte(memory_location, cpu.r.x);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(memory_location, cpu.r.x);
    }
  }
};

var STX_absolute= {
  toString: function() { return "STX" },
  bytes_required:3,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=4;

    var memory_location = (bytes[1]<<8)|bytes[0];
    if(cpu.p.e||cpu.p.x) {
      cpu.mmu.store_byte(memory_location, cpu.r.x);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(memory_location, cpu.r.x);
    }
  }
};

var STA_absolute_long= {
  toString: function() { return "STA" },
  bytes_required:4,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=5;

    var memory_location = (bytes[1]<<8)|bytes[0];
    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.store_byte_long(memory_location, bytes[2], cpu.r.a);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word_long(memory_location, bytes[2], cpu.r.a);
    }
  }
};

var STA_absolute_long_indexed_x= {
  toString: function() { return "STA" },
  bytes_required:4,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=5;

    var memory_location = ((bytes[1]<<8)|bytes[0]) + cpu.r.x;
    if(memory_location & 0x10000) {
      memory_location &= 0xffff;
      bytes[2]++;
    }
    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.store_byte_long(memory_location, bytes[2], cpu.r.a);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_byte_long(memory_location, bytes[2], cpu.r.a&0x00ff);
      memory_location++;
      if(memory_location & 0x10000) {
        bytes[2]++;
      }
      cpu.mmu.store_byte_long(memory_location, bytes[2], cpu.r.a>>8);
    }
  }
};

var STA_absolute= {
  toString: function() { return "STA" },
  bytes_required:3,

  execute: function(cpu, bytes) {
    cpu.cycle_count+=4;

    var memory_location = (bytes[1]<<8)|bytes[0];
    if(cpu.p.e||cpu.p.m) {
      cpu.mmu.store_byte(memory_location, cpu.r.a);
    } else {
      cpu.cycle_count++;

      cpu.mmu.store_word(memory_location, cpu.r.a);
    }
  }
};

var LDA_const= {
  toString: function() { return "LDA" },
  bytes_required: function(cpu) {
    if(cpu.p.e||cpu.p.m) {
      return 2;
    } else {
      return 3;
    }
  },
  execute: function(cpu, bytes) {
    cpu.instruction_details += "<br />Load value to accumulator";
    if(!cpu.instruction_translated) {
      cpu.instruction_details += " (const)";
      cpu.instruction_translate = this.toString() + " #" + bytesToString(bytes);
      cpu.instruction_history += " " + this.toString() + " #" + bytesToString(bytes);
    }
    cpu.cycle_count+=2;

    if(cpu.p.e||cpu.p.m) {
      cpu.r.a = bytes[0];
      cpu.p.n = cpu.r.a >> 7;
    } else {
      cpu.cycle_count++;

      cpu.r.a = (bytes[1]<<8)|bytes[0];
      cpu.p.n = cpu.r.a >> 15;
    }
    cpu_lib.r.p.check_z(cpu, cpu.r.a);
  }
};

var LDA_direct_page = new cpu_lib.addressing.Direct_page(LDA_const);

var LDA_direct_page_indexed_x =
  new cpu_lib.addressing.Direct_page_indexed(LDA_direct_page, 'x');

var LDA_direct_page_indirect =
  new cpu_lib.addressing.Direct_page_indirect(LDA_const);

var LDA_direct_page_indirect_indexed_y =
  new cpu_lib.addressing.Direct_page_indirect_indexed_y(LDA_const);

var LDA_absolute = new cpu_lib.addressing.Absolute(LDA_const);

var LDA_absolute_indexed_x =
  new cpu_lib.addressing.Absolute_indexed_x(LDA_absolute);

var LDA_absolute_indexed_y =
  new cpu_lib.addressing.Absolute_indexed_y(LDA_absolute);

var LDA_stack_relative = new cpu_lib.addressing.Stack_relative(LDA_const);

var LDA_stack_relative_indirect_indexed_y =
  new cpu_lib.addressing.Stack_relative_indirect_indexed_y(LDA_const);

var LDA_absolute_long = new cpu_lib.addressing.Absolute_long(LDA_const);

var LDA_absolute_long_indexed_x =
  new cpu_lib.addressing.Absolute_long_indexed_x(LDA_const);

var LDA_direct_page_indexed_x_indirect =
  new cpu_lib.addressing.Direct_page_indexed_x_indirect(LDA_const);

var LDA_direct_page_indirect_long =
  new cpu_lib.addressing.Direct_page_indirect_long(LDA_const);

var LDA_direct_page_indirect_long_indexed_y =
  new cpu_lib.addressing.Direct_page_indirect_long_indexed_y(LDA_const);

var LDX_const= {
  toString: function() { return "LDX" },
  bytes_required: function(cpu) {
    if(cpu.p.e||cpu.p.x) {
      return 2;
    } else {
      return 3;
    }
  },
  execute: function(cpu, bytes) {
    cpu.instruction_details += "<br />Load value to register X";
    if(!cpu.instruction_translated) {
      cpu.instruction_details += " (const)";
      cpu.instruction_translate = this.toString() + " #" + bytesToString(bytes);
      cpu.instruction_history += " " + this.toString() + " #" + bytesToString(bytes);
    }
    cpu.cycle_count+=2;

    if(cpu.p.e||cpu.p.x) {
      cpu.r.x = bytes[0];
      cpu.p.n = cpu.r.x >> 7;
    } else {
      cpu.cycle_count++;

      cpu.r.x = (bytes[1]<<8)|bytes[0];
      cpu.p.n = cpu.r.x >> 15;
    }
    cpu_lib.r.p.check_z(cpu, cpu.r.x);
  }
};

var LDX_direct_page = new cpu_lib.addressing.Direct_page(LDX_const);

var LDX_direct_page_indexed_y =
  new cpu_lib.addressing.Direct_page_indexed(LDX_direct_page, 'y');

var LDX_absolute = new cpu_lib.addressing.Absolute(LDX_const);

var LDX_absolute_indexed_y =
    new cpu_lib.addressing.Absolute_indexed_y(LDX_absolute);

var SEP = new cpu_lib.r.p.Set_p(1);

var REP = new cpu_lib.r.p.Set_p(0);

var XCE = {
  toString: function() { return "XCE" },
  bytes_required:1,

  execute: function(cpu) {
    cpu.cycle_count+=2;

    var temp = cpu.p.c;
    cpu.p.c = cpu.p.e;
    cpu.p.e = temp;
    if(cpu.p.e) {
      // Switching to emulation mode.
      cpu.r.b = cpu.r.a >> 8;
      cpu.r.a &= 0x00ff;
      cpu.r.s &= 0xff;
    } else {
      // Switching to native mode.
      cpu.r.a = (cpu.r.b<<8) | cpu.r.a;
      cpu.p.m = 1;
      cpu.p.x = 1;
      cpu.r.s |= 0x100;
    }
  }
};

var CLC = new cpu_lib.r.p.Flag_set('c', 0);

var SEC = new cpu_lib.r.p.Flag_set('c', 1);

var CLI = new cpu_lib.r.p.Flag_set('i', 0);

var SEI = new cpu_lib.r.p.Flag_set('i', 1);

var CLD = new cpu_lib.r.p.Flag_set('d', 0);

var SED = new cpu_lib.r.p.Flag_set('d', 1);

var CLV = new cpu_lib.r.p.Flag_set('v', 0);

var XBA = {
  toString: function() { return "XBA" },
  bytes_required:1,

  execute:function(cpu) {
    cpu.cycle_count+=3;

    if(cpu.p.e) {
      cpu.cycle_count+=2;
      var old_a = cpu.r.a;
      cpu.r.a = cpu.r.b;
      cpu.r.b = old_a;

      cpu.p.n = cpu.r.a >> 7;
      cpu_lib.r.p.check_z(cpu, cpu.r.a);
    } else {
      var low_byte = cpu.r.a & 0xff;
      var high_byte = cpu.r.a >> 8;
      cpu.r.a = (low_byte<<8)|high_byte;

      cpu.p.n = high_byte >> 7;
      cpu_lib.r.p.check_z(cpu, high_byte);
    }
  }
};

var MMU = function() {
  this.cpu = {};
  this.memory = { 0: {} };
  this.memory_mapped_io_devices = {};

  this.reset = function() {
    this.memory ={ 0: {} };
  };

  this.add_memory_mapped_io_device = function(write_callback, read_callback,
                                        bank, location) {
    if(typeof this.memory_mapped_io_devices[bank] === 'undefined') {
      this.memory_mapped_io_devices[bank] = {};
    }
    this.memory_mapped_io_devices[bank][location] = { write: write_callback,
                                                      read: read_callback };
  };

  this.pull_byte = function() {
    if(this.cpu.p.e) {
      if(this.cpu.r.s===0xff) {
        this.cpu.r.s = 0;
        return this.read_byte(0x100);
      } else {
        return this.read_byte(0x100|(++this.cpu.r.s));
      }
    } else {
      return this.read_byte(++this.cpu.r.s);
    }
  };

  this.push_byte = function(b) {
    if(this.cpu.p.e) {
      if(this.cpu.r.s===0) {
    this.store_byte(0x100, b);
        this.cpu.r.s = 0xff;
      } else {
        this.store_byte((0x100|(this.cpu.r.s--)), b);
      }
    } else {
      this.store_byte(this.cpu.r.s--, b);
    }
  };

  this.read_byte = function(memory_location) {
    memory_location &= 0xffff; // Make sure the address is 16 bits.

    var device_map_at_bank = this.memory_mapped_io_devices[this.cpu.r.dbr];
    if(typeof device_map_at_bank !== "undefined") {
      var device = device_map_at_bank[memory_location];
      if(typeof device !== "undefined")
        return device.read(this.cpu);
    }
    return this.memory[this.cpu.r.dbr][memory_location];
  };

  this.read_byte_long = function(memory_location, bank) {
    // Make sure addresses given are the proper size.
    memory_location &= 0xffff;
    bank &= 0xff;

    if(typeof this.memory[bank] === 'undefined') {
      this.memory[bank] = {};
    }
    var device_map_at_bank = this.memory_mapped_io_devices[bank];
    if(typeof device_map_at_bank !== "undefined") {
      var device = device_map_at_bank[memory_location];
      if(typeof device !== "undefined")
        return device.read(this.cpu);
    }
    return this.memory[bank][memory_location];
  };

  this.store_byte = function(memory_location, b) {
    memory_location &= 0xffff; // Make sure the address is 16 bits
    b &= 0xff; // Make sure the byte is actually a byte long

    var device_map_at_bank = this.memory_mapped_io_devices[this.cpu.r.dbr];
    if(typeof device_map_at_bank !== "undefined") {
      var device = device_map_at_bank[memory_location];
      if(typeof device !== "undefined")
        device.write(this.cpu, b);
    }
    this.memory[this.cpu.r.dbr][memory_location] = b;
  };

  this.store_byte_long = function(memory_location, bank, b) {
    // Make sure addresses and byte given are the proper size.
    memory_location &= 0xffff;
    bank &= 0xff;
    b &= 0xff;

    if(typeof this.memory[bank] === 'undefined') {
      this.memory[bank] = {};
    }
    var device_map_at_bank = this.memory_mapped_io_devices[bank];
    if(typeof device_map_at_bank !== "undefined") {
      var device = device_map_at_bank[memory_location];
      if(typeof device !== "undefined")
        device.write(this.cpu, b);
    }
    this.memory[bank][memory_location] = b;
  };

  this.read_word = function(memory_location) {
    return (this.read_byte(memory_location+1)<<8) |
           this.read_byte(memory_location);
  };

  this.read_word_long = function(memory_location, bank) {
    return (this.read_byte_long(memory_location+1, bank)<<8) |
           this.read_byte_long(memory_location, bank);
  };

  this.store_word = function(memory_location, word_or_low_byte, high_byte) {
    // If no high_byte is given then assume word_or_low_byte is a word.
    if(typeof high_byte === 'undefined') {
      this.store_byte(memory_location, word_or_low_byte&0xff);
      this.store_byte(memory_location+1, word_or_low_byte>>8);
    } else {
      this.store_byte(memory_location, word_or_low_byte);
      this.store_byte(memory_location+1, high_byte);
    }
  };

  this.store_word_long = function(memory_location, bank, word_or_low_byte,
                                  high_byte) {
    // If no high_byte is given then assume word_or_low_byte is a word.
    if(typeof high_byte === 'undefined') {
      this.store_byte_long(memory_location, bank, word_or_low_byte&0xff);
      this.store_byte_long(memory_location+1, bank, word_or_low_byte>>8);
    } else {
      this.store_byte_long(memory_location, bank, word_or_low_byte);
      this.store_byte_long(memory_location+1, bank, high_byte);
    }
  };
};

window.CPU_65816 = function() {

  // Instruction & Instruction details to print
  this.instruction = "";
  this.instruction_details = "";
  this.instruction_translate = "";
  this.instruction_history = "";
  this.instruction_translated = false;

  // Registers
  this.r = {
    a:0,     // Accumulator
    b:0,     // "Hidden" Accumulator Register(high byte in 8-bit mode)
    x:0,     // X Index Register
    y:0,     // Y Index Register
    d:0,     // Direct Page Register
    s:0xff,  // Stack Pointer
    pc:0,    // Program Counter
    dbr:0,   // Data Bank Register
    k:0      // Program Bank Register
  };

  // P register flags.
  this.p = {
    e:1, // Emulator                  (0 = native mode)
    c:0, // Carry                     (1 = carry)
    z:0, // Zero                      (1 = zero)
    i:0, // IRQ Disable               (1 = disabled)
    d:0, // Decimal Mode              (1 = decimal, 0 = binary)
    x:0, // Index Register Select     (1 = 8-bit, 0 = 16-bit)
    m:0, // Memory/Accumulator Select (1 = 8-bit, 0 = 16-bit)
    v:0, // Overflow                  (1 = overflow)
    n:0  // Negative                  (1 = negative)
  };

  this.INTERRUPT = { NO_INTERRUPT: 0, NMI: 1, RESET: 2, ABORT: 3, COP: 4,
                     IRQ: 5, BRK: 6 };

  this.interrupt = this.INTERRUPT.NO_INTERRUPT;

  // This is used to keep the cpu going if started with start().
  this.executing = false;

  // This is set by the WAI operation to stop execution until an interrupt
  // is received.
  this.waiting = false;

  // This is set by the STP operation to stop execution until a RESET
  // interrupt is received.
  this.stopped = false;

  this.raise_interrupt = function(i) {
    if(this.waiting) {
      this.waiting = false;
      if(this.p.i) {
        if(i===this.INTERRUPT.IRQ) {
          i = this.INTERRUPT.NO_INTERRUPT;
        }
      }
      this.interrupt = i;
      this.start();
    } else if(this.stopped&&(i===this.INTERRUPT.RESET)) {
      this.stopped = false;
      this.start();
    } else {
      this.interrupt = i;
    }
  };

  this.cycle_count = 0;

  this.mmu = new MMU();
  this.mmu.cpu = this;

  this.opcode_map = { 0xfb : XCE, 0x18 : CLC, 0x78 : SEI, 0x38 : SEC,
                      0x58 : CLI, 0xc2 : REP, 0xe2 : SEP, 0xd8 : CLD,
                      0xf8 : SED, 0xb8 : CLV, 0xeb : XBA, 0xa9 : LDA_const,
                      0xad : LDA_absolute, 0xaf : LDA_absolute_long,
                      0xbf : LDA_absolute_long_indexed_x,
                      0xa5 : LDA_direct_page, 0xbd : LDA_absolute_indexed_x,
                      0xb5 : LDA_direct_page_indexed_x,
                      0xb9 : LDA_absolute_indexed_y,
                      0xa1 : LDA_direct_page_indexed_x_indirect,
                      0xb2 : LDA_direct_page_indirect,
                      0xa7 : LDA_direct_page_indirect_long,
                      0xb7 : LDA_direct_page_indirect_long_indexed_y,
                      0xb1 : LDA_direct_page_indirect_indexed_y,
                      0xa3 : LDA_stack_relative,
                      0xb3 : LDA_stack_relative_indirect_indexed_y,
                      0xa2 : LDX_const,
                      0xae : LDX_absolute, 0xa6 : LDX_direct_page,
                      0xa0 : LDY_const, 0xbc : LDY_absolute_indexed_x,
                      0xb4 : LDY_direct_page_indexed_x,
                      0xbe : LDX_absolute_indexed_y,
                      0xb6 : LDX_direct_page_indexed_y,
                      0xac : LDY_absolute, 0xa4 : LDY_direct_page, 0xea : NOP,
                      0x8d : STA_absolute, 0x85 : STA_direct_page,
                      0x8f : STA_absolute_long,
                      0x9f : STA_absolute_long_indexed_x,
                      0x81 : STA_direct_page_indexed_x_indirect,
                      0x92 : STA_direct_page_indirect,
                      0x87 : STA_direct_page_indirect_long,
                      0x97 : STA_direct_page_indirect_long_indexed_y,
                      0x91 : STA_direct_page_indirect_indexed_y,
                      0x9d : STA_absolute_indexed_x,
                      0x99 : STA_absolute_indexed_y,
                      0x95 : STA_direct_page_indexed_x,
                      0x83 : STA_stack_relative,
                      0x93 : STA_stack_relative_indirect_indexed_y,
                      0x8e : STX_absolute, 0x86 : STX_direct_page,
                      0x96 : STX_direct_page_indexed_y,
                      0x8c : STY_absolute, 0x84 : STY_direct_page,
                      0x94 : STY_direct_page_indexed_x,
                      0x1a : INC_accumulator, 0xe6 : INC_direct_page,
                      0xee : INC_absolute, 0xf6 : INC_direct_page_indexed_x,
                      0xfe : INC_absolute_indexed_x,
                      0xe8 : INX, 0xc8 : INY,
                      0x3a : DEC_accumulator, 0xce : DEC_absolute,
                      0xde : DEC_absolute_indexed_x,
                      0xc6 : DEC_direct_page, 0xca : DEX, 0x88 : DEY,
                      0xd6 : DEC_direct_page_indexed_x,
                      0x9c : STZ_absolute, 0x64 : STZ_direct_page,
                      0x9e : STZ_absolute_indexed_x,
                      0x74 : STZ_direct_page_indexed_x, 0x9b : TXY,
                      0xbb : TYX, 0xaa : TAX, 0xa8 : TAY, 0x8a : TXA,
                      0x98 : TYA, 0x5b : TCD, 0x7b : TDC, 0x1b : TCS,
                      0x3b : TSC, 0x4c : JMP_absolute,
                      0x5c : JMP_absolute_long,
                      0xdc : JMP_absolute_indirect_long,
                      0x7c : JMP_absolute_indexed_x_indirect,
                      0x6c : JMP_absolute_indirect, 0x80 : BRA, 0x82 : BRL,
                      0xf0 : BEQ, 0xd0 : BNE, 0x90 : BCC, 0xb0 : BCS,
                      0x50 : BVC, 0x70 : BVS, 0x10 : BPL, 0x30 : BMI,
                      0x69 : ADC_const, 0x6d : ADC_absolute,
                      0x61 : ADC_direct_page_indexed_x_indirect,
                      0x6f : ADC_absolute_long,
                      0x7f : ADC_absolute_long_indexed_x,
                      0x65 : ADC_direct_page, 0x72 : ADC_direct_page_indirect,
                      0x67 : ADC_direct_page_indirect_long,
                      0x77 : ADC_direct_page_indirect_long_indexed_y,
                      0x71 : ADC_direct_page_indirect_indexed_y,
                      0x7d : ADC_absolute_indexed_x,
                      0x79 : ADC_absolute_indexed_y,
                      0x75 : ADC_direct_page_indexed_x,
                      0x63 : ADC_stack_relative,
                      0x73 : ADC_stack_relative_indirect_indexed_y,
                      0xe9 : SBC_const,
                      0xed : SBC_absolute, 0xe5 : SBC_direct_page,
                      0xef : SBC_absolute_long,
                      0xff : SBC_absolute_long_indexed_x,
                      0xf2 : SBC_direct_page_indirect,
                      0xe1 : SBC_direct_page_indexed_x_indirect,
                      0xe7 : SBC_direct_page_indirect_long,
                      0xf7 : SBC_direct_page_indirect_long_indexed_y,
                      0xf1 : SBC_direct_page_indirect_indexed_y,
                      0xfd : SBC_absolute_indexed_x,
                      0xf9 : SBC_absolute_indexed_y,
                      0xf5 : SBC_direct_page_indexed_x,
                      0xe3 : SBC_stack_relative,
                      0xf3 : SBC_stack_relative_indirect_indexed_y,
                      0xc9 : CMP_const, 0xc5 : CMP_direct_page,
                      0xcd : CMP_absolute, 0xd2 : CMP_direct_page_indirect,
                      0xcf : CMP_absolute_long,
                      0xdf : CMP_absolute_long_indexed_x,
                      0xc7 : CMP_direct_page_indirect_long,
                      0xd7 : CMP_direct_page_indirect_long_indexed_y,
                      0xd5 : CMP_direct_page_indexed_x,
                      0xc1 : CMP_direct_page_indexed_x_indirect,
                      0xdd : CMP_absolute_indexed_x,
                      0xd9 : CMP_absolute_indexed_y,
                      0xd1 : CMP_direct_page_indirect_indexed_y,
                      0xc3 : CMP_stack_relative,
                      0xd3 : CMP_stack_relative_indirect_indexed_y,
                      0xe0 : CPX_const,
                      0xec : CPX_absolute, 0xe4 : CPX_direct_page,
                      0xc0 : CPY_const, 0xcc : CPY_absolute,
                      0xc4 : CPY_direct_page, 0x29 : AND_const,
                      0x2d : AND_absolute, 0x25 : AND_direct_page,
                      0x2f : AND_absolute_long,
                      0x3f : AND_absolute_long_indexed_x,
                      0x21 : AND_direct_page_indexed_x_indirect,
                      0x32 : AND_direct_page_indirect,
                      0x27 : AND_direct_page_indirect_long,
                      0x37 : AND_direct_page_indirect_long_indexed_y,
                      0x31 : AND_direct_page_indirect_indexed_y,
                      0x3d : AND_absolute_indexed_x,
                      0x39 : AND_absolute_indexed_y,
                      0x35 : AND_direct_page_indexed_x,
                      0x23 : AND_stack_relative,
                      0x33 : AND_stack_relative_indirect_indexed_y,
                      0x09 : ORA_const, 0x0f : ORA_absolute_long,
                      0x0d : ORA_absolute, 0x05 : ORA_direct_page,
                      0x1f : ORA_absolute_long_indexed_x,
                      0x12 : ORA_direct_page_indirect,
                      0x01 : ORA_direct_page_indexed_x_indirect,
                      0x07 : ORA_direct_page_indirect_long,
                      0x17 : ORA_direct_page_indirect_long_indexed_y,
                      0x11 : ORA_direct_page_indirect_indexed_y,
                      0x1d : ORA_absolute_indexed_x,
                      0x19 : ORA_absolute_indexed_y,
                      0x15 : ORA_direct_page_indexed_x,
                      0x03 : ORA_stack_relative,
                      0x13 : ORA_stack_relative_indirect_indexed_y,
                      0x49 : EOR_const, 0x4d : EOR_absolute,
                      0x4f : EOR_absolute_long,
                      0x5f : EOR_absolute_long_indexed_x,
                      0x45 : EOR_direct_page,
                      0x52 : EOR_direct_page_indirect,
                      0x41 : EOR_direct_page_indexed_x_indirect,
                      0x47 : EOR_direct_page_indirect_long,
                      0x57 : EOR_direct_page_indirect_long_indexed_y,
                      0x51 : EOR_direct_page_indirect_indexed_y,
                      0x5d : EOR_absolute_indexed_x,
                      0x59 : EOR_absolute_indexed_y,
                      0x55 : EOR_direct_page_indexed_x,
                      0x43 : EOR_stack_relative,
                      0x53 : EOR_stack_relative_indirect_indexed_y,
                      0x4a : LSR_accumulator, 0x4e : LSR_absolute,
                      0x46 : LSR_direct_page, 0x5e : LSR_absolute_indexed_x,
                      0x56 : LSR_direct_page_indexed_x, 0x0a : ASL_accumulator,
                      0x0e : ASL_absolute, 0x06 : ASL_direct_page,
                      0x1e : ASL_absolute_indexed_x,
                      0x16 : ASL_direct_page_indexed_x, 0x2a : ROL_accumulator,
                      0x2e : ROL_absolute, 0x26 : ROL_direct_page,
                      0x3e : ROL_absolute_indexed_x,
                      0x36 : ROL_direct_page_indexed_x, 0x6a : ROR_accumulator,
                      0x6e : ROR_absolute, 0x66 : ROR_direct_page,
                      0x7e : ROR_absolute_indexed_x,
                      0x76 : ROR_direct_page_indexed_x,
                      0x48 : PHA, 0x68 : PLA, 0x5a : PHY, 0x7a : PLY,
                      0xda : PHX, 0xfa : PLX, 0x08 : PHP, 0x28 : PLP,
                      0xf4 : PEA, 0xd4 : PEI, 0x8b : PHB, 0xab : PLB,
                      0x4b : PHK, 0x0b : PHD, 0x2b : PLD, 0x62 : PER,
                      0x20 : JSR, 0xfc : JSR_absolute_indexed_x_indirect,
                      0x60 : RTS, 0x22 : JSL, 0x6b : RTL,
                      0x54 : MVN, 0x44 : MVP, 0x00 : BRK, 0x40 : RTI,
                      0x02 : COP, 0x89 : BIT_const, 0x2c : BIT_absolute,
                      0x24 : BIT_direct_page,
                      0x3c : BIT_absolute_indexed_x,
                      0x34 : BIT_direct_page_indexed_x,
                      0x0c : TSB_absolute, 0x04 : TSB_direct_page,
                      0x1c : TRB_absolute, 0x14 : TRB_direct_page,
                      0x9a : TXS, 0xba : TSX, 0x42: WDM, 0xcb : WAI,
                      0xdb : STP };

  /**
   * Load given program into memory and prepare for execution.
   * raw_hex could either be a string of hex numbers or an array
   * of bytes.
   */
  this.load_binary = function(raw_hex, memory_location_start, bank) {
    var byte_buffer = [],
        i = 0,
        starting_memory = memory_location_start;

    if(typeof bank === "undefined") {
      bank = 0;
    }

    if(typeof raw_hex === 'string') {
      for(;i < raw_hex.length; i++) {
        byte_buffer.push(raw_hex.charAt(i));
        if(byte_buffer.length===2) {
          this.mmu.store_byte_long(memory_location_start, bank,
                                   parseInt(byte_buffer[0]+byte_buffer[1],
                                            16));
          memory_location_start++;
          byte_buffer = [];
        }
      }
    } else {
      for(;i < raw_hex.length; i++) {
        this.mmu.store_byte_long(memory_location_start, bank, raw_hex[i]);
        memory_location_start++;
      }
    }
    this.r.pc = starting_memory;
  };

  /**
   * Step through the processing of a single instruction from the current
   * location of the program counter.
   */
  this.step = function() {
    this.instruction_translated = false;
    if(this.interrupt&&(!this.p.i||(this.interrupt===this.INTERRUPT.NMI))) {
      // Load the related interrupt vector in page 0xff of bank zero.
      if(!this.p.e) {
        this.mmu.push_byte(this.r.k);
      }
      this.mmu.push_byte(this.r.pc>>8);
      this.mmu.push_byte(this.r.pc&0xff);
      var p_byte = (this.p.n<<7)|(this.p.v<<6)|(this.p.m<<5)|(this.p.x<<4)|
                   (this.p.d<<3)|(this.p.i<<2)|(this.p.z<<1)|this.p.c;
      this.mmu.push_byte(p_byte);
      if(!this.p.e)
        this.p.d = 0;
      this.p.i = 1;
      this.r.k = 0;

      // Look for where to jump to for the interrupt.
      if(this.p.e) {
        // NMI
        if(this.interrupt===this.INTERRUPT.NMI) {
          this.r.pc = this.mmu.read_word_long(0xfffa, 0);
        // RESET
        } else if(this.interrupt===this.INTERRUPT.RESET) {
          this.r.pc = this.mmu.read_word_long(0xfffc, 0);
        // ABORT
        } else if(this.interrupt===this.INTERRUPT.ABORT) {
          this.r.pc = this.mmu.read_word_long(0xfff8, 0);
        // COP
        } else if(this.interrupt===this.INTERRUPT.COP) {
          this.r.pc = this.mmu.read_word_long(0xfff4, 0);
        // IRQ or BRK
        } else if(this.interrupt===this.INTERRUPT.IRQ ||
                  this.interrupt===this.INTERRUPT.BRK) {
          this.r.pc = this.mmu.read_word_long(0xfffe, 0);
        }
      } else {
        // NMI
        if(this.interrupt===this.INTERRUPT.NMI) {
          this.r.pc = this.mmu.read_word_long(0xffea, 0);
        // ABORT
        } else if(this.interrupt===this.INTERRUPT.ABORT) {
          this.r.pc = this.mmu.read_word_long(0xffe8, 0);
        // COP
        } else if(this.interrupt===this.INTERRUPT.COP) {
          this.r.pc = this.mmu.read_word_long(0xffe4, 0);
        // IRQ
        } else if(this.interrupt===this.INTERRUPT.IRQ) {
          this.r.pc = this.mmu.read_word_long(0xffee, 0);
        // BRK
        } else if(this.interrupt===this.INTERRUPT.BRK) {
          this.r.pc = this.mmu.read_word_long(0xffe6, 0);
        }
      }

      this.interrupt = this.INTERRUPT.NO_INTERRUPT;
    }

    var b = this.mmu.read_byte_long(this.r.pc, this.r.k);
    this.r.pc++;

    // If we reach the end of the code then stop everything.
    if(typeof b === "undefined") {
      this.executing = false;
      this.instruction_details = "opcode b is undefined";
      return;
    }
    this.instruction = b.toString(16).toUpperCase();
    var operation = this.opcode_map[b];
    this.instruction_translate = operation.toString();

    // Minus One on PC reg because we already read the current instruction
    this.instruction_history
      += bytesToString(this.r.k) + "/" + bytesToString(this.r.pc-1) + ": "
      + bytesToString(b);

    // bytes_required can either be a number, or a function that resolves
    // to a number using some aspect of the cpu.
    var bytes_required = operation.bytes_required;
    if(typeof bytes_required === 'function') {
      bytes_required = bytes_required(this);
    }
    this.instruction_details = bytes_required + " bytes read";

    if(bytes_required===1) {
      operation.execute(this);
    } else {
      var bytes = [];
      for(var i = 1; i < bytes_required; i++) {
        var bytes_read = this.mmu.read_byte_long(this.r.pc, this.r.k);
        bytes.push(bytes_read);
        this.instruction += " " + bytesToString(bytes_read);
        this.instruction_history += " " + bytesToString(bytes_read);
        this.r.pc++;
      }
      operation.execute(this,bytes);
    }
    this.instruction_history += "<br />";

    if(this.waiting||this.stopped)
      this.executing = false;
  };

  this.execute = function(start_address, max_cycles_per_second) {
    // Default to 1MHz if no number given.
    if(typeof max_cycles_per_second === "undefined") {
      max_cycles_per_second = 1000000;
    }

    this.r.pc = start_address;
    this.timer_run(max_cycles_per_second, 1000);
  };

  this.timer_run = function(max_cycles_per_period, period) {
    var start = new Date().getTime();
    this.executing = true;
    while(this.executing) {
      this.step();
      // If execution stopped other than because of the cycle count
      if(!this.executing) {
        return;
      }
      if(this.cycle_count>=max_cycles_per_period) {
        this.executing = false;
        var now = new Date().getTime();
        var wait = period - (now - start);
        this.cycle_count = 0;
        if(wait>0) {
          setTimeout(this.timer_run.bind(this, max_cycles_per_period, period), wait);
        } else {
          this.timer_run(max_cycles_per_period, period);
        }
      }
    }
  };

  this.reset = function() {
    this.executing = false;
    this.waiting = false;
    this.stopped = false;
    this.instruction = "";
    this.instruction_translate = "";
    this.instruction_details = "";
    this.instruction_history = "";

    this.interrupt = this.INTERRUPT.NO_INTERRUPT;
    this.r = { a:0, b:0, x:0, y:0, d:0, s:0xff, pc:0, dbr:0, k:0 };
    this.p = { e:1, c:0, z:0, i:0, d:0, x:0, m:0, v:0, n:0 };
    this.mmu.reset();
    this.cycle_count = 0;
  };
};
})((typeof module !== 'undefined' &&
    typeof module.exports !== 'undefined')?module.exports:this);