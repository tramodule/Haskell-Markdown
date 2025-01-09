import pluginChecker from "vite-plugin-checker";
import { UserConfig } from "vite";

const config: UserConfig = {
    plugins: [pluginChecker({ typescript: true })],
    server: {
        proxy: {
            "/api": {
                target: "http://127.0.0.1:3000/",
                changeOrigin: true,
            },
        },
    },
};

const getConfig = () => config;

export default getConfig;
