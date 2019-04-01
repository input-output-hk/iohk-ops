self: super:
{
  nginxMainline = super.nginxMainline.override {
    modules = super.nginxMainline.modules ++ [ super.nginxModules.vts ];
  };

  nginxStable = super.nginxStable.override {
    modules = super.nginxStable.modules ++ [ super.nginxModules.vts ];
  };
}
