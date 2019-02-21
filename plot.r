g_range <- range(0, df$mq2, df$mq3)
plot(df$mq2, type="o", col="blue",ylim=g_range)
lines(df$mq3, type="o", pch=22, lty=2, col="red")