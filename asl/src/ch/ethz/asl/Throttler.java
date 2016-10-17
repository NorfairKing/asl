package ch.ethz.asl;

import java.util.concurrent.atomic.AtomicLong;

public class Throttler {
    private final int rate;
    private final AtomicLong counter;
    public Throttler(int rate){
        this.rate = rate;
        this.counter = new AtomicLong(0);
    }
    public boolean countAndCheck(){
        return counter.getAndIncrement() % rate == 0;
    }
}
