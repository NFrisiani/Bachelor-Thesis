x = 0:0.1:10;
y = sigmf(x,[2 4]);
plot(x,y)
xlabel('Stride (log scale)')
ylabel('MAT (nanoseconds)')
ylim([-0.05 1.05])