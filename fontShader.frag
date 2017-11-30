uniform sampler2D fontmap;

void main(void) {
    vec2 texCoord = vec2(gl_TexCoord[0]);
    float alpha = texture2D(fontmap, texCoord).r;
    gl_FragColor = vec4(gl_Color.rgb, alpha);
}
