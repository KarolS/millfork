package com.grapeshot.halfnes;

import com.grapeshot.halfnes.mappers.Mapper;
import millfork.output.MemoryBank;

import java.util.prefs.Preferences;

/**
 * Since the original CPURAM class was a convoluted mess of dependencies,
 * I overrode it with mine that has only few pieces of junk glue to make it work
 * @author Karol Stasiak
 */
@SuppressWarnings("unused")
public class CPURAM {

    static {
        Preferences preferences = Preferences.userNodeForPackage(NES.class);
        preferences.putBoolean("soundEnable", false);
    }

    private final MemoryBank mem;

    // required by the CPU class for some reason
    public Mapper mapper = new Mapper() {
        @Override
        public TVType getTVType() {
            // the base class returns null, but this can't be null
            return TVType.DENDY;
        }
    };
    // required by the CPU class for some reason
    public APU apu = new APU(null, null, this);

    public CPURAM(MemoryBank mem) {
        boolean[] readable = mem.readable();
        boolean[] writeable = mem.writeable();
        for (int i = 0xfffe; i >= 0; i--) {
            if (readable[i]) {
                // allow for dummy fetches by implied instructions
                readable[i + 1] = true;
            }
        }
        readable[0] = true;
        readable[1] = true;
        readable[2] = true;
        for (int i = 0x100; i <= 0x1ff; i++) {
            readable[i] = true;
            writeable[i] = true;
        }
        for (int i = 0x4000; i <= 0x407f; i++) {
            readable[i] = true;
            writeable[i] = true;
        }
        for (int i = 0xc000; i <= 0xcfff; i++) {
            readable[i] = true;
            writeable[i] = true;
        }
        for (int i = 0xfffa; i <= 0xffff; i++) {
            readable[i] = true;
            writeable[i] = true;
        }
        this.mem = mem;
    }


    public final int read(int addr) {
        addr &= 0xffff;
        if (!mem.readable()[addr]) {
            throw new RuntimeException("Can't read from $" + Integer.toHexString(addr));
        }
        return mem.output()[addr] & 0xff;
    }

    public final void write(int addr, int data) {
        addr &= 0xffff;
        if (!mem.writeable()[addr]) {
            throw new RuntimeException("Can't write $" + Integer.toHexString(data & 0xff) + "to $" + Integer.toHexString(addr));
        }
        mem.output()[addr] = (byte) data;
    }

}
