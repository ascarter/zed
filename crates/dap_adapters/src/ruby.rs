use crate::*;
use dap::{adapters::TcpArguments, transport::TcpTransport};
use gpui::AsyncApp;
use std::{ffi::OsStr, net::Ipv4Addr, path::PathBuf, sync::Arc};

pub(crate) struct RubyDebugAdapter {
    port: u16,
    host: Ipv4Addr,
    timeout: Option<u64>,
}

impl RubyDebugAdapter {
    const ADAPTER_NAME: &'static str = "rdbg";

    pub(crate) async fn new(host: TCPHost) -> Result<Self> {
        Ok(RubyDebugAdapter {
            port: TcpTransport::port(&host).await?,
            host: host.host(),
            timeout: host.timeout,
        })
    }
}

#[async_trait(?Send)]
impl DebugAdapter for RubyDebugAdapter {
    fn name(&self) -> DebugAdapterName {
        DebugAdapterName(Self::ADAPTER_NAME.into())
    }

    async fn get_binary(
        &self,
        delegate: &dyn DapDelegate,
        config: &DebugAdapterConfig,
        user_installed_path: Option<PathBuf>,
        cx: &mut AsyncApp,
    ) -> Result<DebugAdapterBinary> {
        self.get_installed_binary(delegate, config, user_installed_path, cx)
            .await
    }

    async fn fetch_latest_adapter_version(
        &self,
        _delegate: &dyn DapDelegate,
    ) -> Result<AdapterVersion> {
        unimplemented!("This adapter is used from path for now");
    }

    async fn install_binary(
        &self,
        _version: AdapterVersion,
        _delegate: &dyn DapDelegate,
    ) -> Result<()> {
        unimplemented!("This adapter is used from path for now");
    }

    async fn get_installed_binary(
        &self,
        delegate: &dyn DapDelegate,
        config: &DebugAdapterConfig,
        _: Option<PathBuf>,
        _: &mut AsyncApp,
    ) -> Result<DebugAdapterBinary> {
        let rdbg_path = delegate
            .which(OsStr::new("rdbg"))
            .and_then(|p| p.to_str().map(|p| p.to_string()))
            .ok_or(anyhow!("rdbg not found in path"))?;

        Ok(DebugAdapterBinary {
            command: rdbg_path,
            arguments: Some(vec![
                "--open".into(),
                "--host".into(),
                self.host.to_string().into(),
                "--port".into(),
                self.port.to_string().into(),
                config.program.clone().expect("REASON").to_string().into(),
            ]),
            connection: Some(TcpArguments {
                port: Some(self.port),
                host: Ipv4Addr::LOCALHOST,
                timeout: self.timeout,
            }),
            cwd: config.cwd.clone(),
            envs: None,
            #[cfg(any(test, feature = "test-support"))]
            is_fake: false,
        })
    }

    fn request_args(&self, config: &DebugAdapterConfig) -> Value {
        json!({
            "program": config.program,
            "cwd": config.cwd,
            "request": "launch",
            "subProcess": true,
        })
    }
}
